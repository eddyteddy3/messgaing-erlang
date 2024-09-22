-module(benchmark).

-compile(export_all).

%% Fibonacci
fib(0) -> 1;
fib(1) -> 1;
fib(N) -> fib(N - 1) + fib(N - 2).

%% Benchmark helpers

% Recommendation: run each test at least 30 times to get statistically relevant
% results.
run_benchmark(Name, Fun, Times) ->
    ThisPid = self(),
    lists:foreach(fun (N) ->
        % Recommendation: to make the test fair, each run executes in its own,
        % newly created Erlang process. Otherwise, if all tests run in the same
        % process, the later tests start out with larger heap sizes and
        % therefore probably do fewer garbage collections. Also consider
        % restarting the Erlang emulator between each test.
        % Source: http://erlang.org/doc/efficiency_guide/profiling.html
        spawn_link(fun () ->
            run_benchmark_once(Name, Fun, N),
            ThisPid ! done
        end),
        receive done ->
            ok
        end
    end, lists:seq(1, Times)).

run_benchmark_once(Name, Fun, N) ->
    % io:format("Starting benchmark ~s: ~p~n", [Name, N]),

    % Start timers
    % Tips:
    % * Wall clock time measures the actual time spent on the benchmark.
    %   I/O, swapping, and other activities in the operating system kernel are
    %   included in the measurements. This can lead to larger variations.
    %   os:timestamp() is more precise (microseconds) than
    %   statistics(wall_clock) (milliseconds)
    % * CPU time measures the actual time spent on this program, summed for all
    %   threads. Time spent in the operating system kernel (such as swapping and
    %   I/O) is not included. This leads to smaller variations but is
    %   misleading.
    StartTime = os:timestamp(), % Wall clock time
    % statistics(runtime),       % CPU time, summed for all threads

    % Run
    Fun(),

    % Get and print statistics
    % Recommendation [1]:
    % The granularity of both measurement types can be high. Therefore, ensure
    % that each individual measurement lasts for at least several seconds.
    % [1] http://erlang.org/doc/efficiency_guide/profiling.html
    WallClockTime = timer:now_diff(os:timestamp(), StartTime),
    % {_, CpuTime} = statistics(runtime),
    io:format("wallclock: ~p~n", [WallClockTime / 1000.0]),
    % io:format("CPU time = ~p ms~n", [CpuTime]),
    io:format("~s done~n", [Name]).

%% Benchmarks
% Below are some example benchmarks. Extend these to test the best and worst
% case of your implementation, some typical scenarios you imagine, or some
% extreme scenarios.

test_fib() ->
    io:format("Parameters:~n"),
    io:format("~n"),
    run_benchmark("fib", fun test_fib_benchmark/0, 30).

test_fib_benchmark() ->
    % Spawn 64 processes that each compute the 30th Fibonacci number.
    BenchmarkPid = self(),
    Pids = [spawn(fun () ->
        fib(30),
        BenchmarkPid ! done
    end) || _ <- lists:seq(1, 64)],
    lists:foreach(fun (_) ->
        receive done ->
            ok
        end
    end, Pids).

% Creates a server with 1000 users following 25 others and sending 100 messages.
%
% Note that this code depends on the implementation of the server. You will need to
% change it if you change the representation of the data in the server.
initialize_server() ->
    initialize_server_with_params(1000, 25, 100).

initialize_server_with_params(NumUsers, NumSubs, NumMessages) ->
    % Seed random number generator to get reproducible results.
    rand:seed_s(exsplus, {0, 0, 0}),
    % Parameters
    NumberOfUsers = NumUsers,
    NumberOfSubscriptions = NumSubs,
    NumberOfMessages = NumMessages,
    % io:format("Parameters:~n"),
    % io:format("Number of users: ~p~n", [NumberOfUsers]),
    % io:format("Number of subscriptions: ~p~n", [NumberOfSubscriptions]),
    % io:format("Number of messages: ~p~n", [NumberOfMessages]),
    
    % Generate user names: just the numbers from 1 to NumberOfUsers, as strings.
    % Note: integer_to_list convert an integer to a string, e.g. 123 to "123".
    % Note: the syntax [F(X) || X <- L] is a list comprehension. It generates a list
    % by applying F to each element of L. It is equivalent to
    % lists:map(fun (X) -> F(X) end, L).
    UserNames = [integer_to_list(I) || I <- lists:seq(1, NumberOfUsers)],
    % Generate users dict.
    Users = dict:from_list(lists:map(fun (Name) ->
        % Random subscriptions.
        Subscriptions = [pick_random(UserNames) || _ <- lists:seq(1, NumberOfSubscriptions)],
        % Random messages.
        Messages = [generate_message(Name, I) || I <- lists:seq(1, NumberOfMessages)],
        UserPid = spawn(user_process, start, [Name]),
        User = {user, Name, pid, UserPid, sets:from_list(Subscriptions), Messages},
        {Name, User} % {key, value} for dict.
        end,
        UserNames)),

    ServerPid = server_centralized:initialize_with(Users),
    % io:format("Server PID: ~p~n", [ServerPid]),
    % io:format("~n"),
    {ServerPid, UserNames}.

% Pick a random element from a list.
pick_random(List) ->
    lists:nth(rand:uniform(length(List)), List).

% Generate a random message `I` for `UserName`.
generate_message(UserName, I) ->
    Text = "Message " ++ integer_to_list(I) ++ " from " ++ UserName,
    {message, UserName, Text, os:system_time()}.

% Get timeline of 10000 users (repeated 30 times).
test_timeline(NumProcesses) ->
    % {ServerPid, UserNames} = initialize_server_with_params(5, 2, 10),

    {ServerPid, UserNames} = initialize_server(),

    % io:format("ServerPiD: ~p.", [ServerPid]),
    % io:format("UserName: ~p.", [UserName]),
    % io:format(""),

    run_benchmark("timeline",
        fun () ->
            lists:foreach(fun (_) ->
                server:get_timeline(ServerPid, pick_random(UserNames))
            end,
            lists:seq(1, NumProcesses))
        end,
        30).

% Sequential send message for 10000 users.
test_send_message(NumProcesses) ->
    {ServerPid, UserNames} = initialize_server(),
    io:format("Benchmarking send_message...~n"),
    run_benchmark("send_message_sequential",
        fun () ->
            lists:foreach(fun (_) ->
                server:send_message(ServerPid, pick_random(UserNames), "Test")
            end,
            lists:seq(1, NumProcesses))
        end,
        30).

test_send_message_with_timeline() ->
    NumThreads = 1,
    NumProcesses = 1,
    NumOfUser = 1000,
    NumMessages = 500,
    NumOfSubs = 0,
    {ServerPid, UserNames} = initialize_server_with_params(NumOfUser, NumOfSubs, NumMessages),
    % {ServerPid, UserNames} = initialize_server(),

    io:format("test_get_timeline_before"),
    io:format("~n"),

    run_benchmark("test_get_timeline_before",
     fun () ->
        BenchmarkPid = self(),
        PIDs = [
            spawn(fun () ->
                lists:foreach(
                    fun (_) ->
                        server:get_timeline(ServerPid, pick_random(UserNames))
                    end,
                    lists:seq(1, NumProcesses div NumThreads)
                ),
                BenchmarkPid ! done

            end) || _ <- lists:seq(1, NumThreads)
            ],

        lists:foreach(fun (_) -> receive done -> ok end end, PIDs)
    end,
    10),
    
    io:format("send_message"),
    io:format("~n"),

    run_benchmark("send_message",
     fun () ->
        BenchmarkPid = self(),
        PIDs = [
            spawn(fun () ->
                lists:foreach(
                    fun (_) ->
                        server:follow(ServerPid, pick_random(UserNames), pick_random(UserNames))
                    end,
                    lists:seq(1, NumProcesses div NumThreads)
                ),
                
                lists:foreach(
                    fun (_) ->
                        server:send_message(ServerPid, pick_random(UserNames), "Test")
                    end,
                    lists:seq(1, NumProcesses div NumThreads)
                ),
                BenchmarkPid ! done

            end) || _ <- lists:seq(1, NumThreads)
            ],

        lists:foreach(fun (_) -> receive done -> ok end end, PIDs)
    end,
    10),
    
    io:format("test_get_timeline_after"),
    io:format("~n"),

    run_benchmark("test_get_timeline_after",
     fun () ->
        BenchmarkPid = self(),
        PIDs = [
            spawn(fun () ->
                lists:foreach(
                    fun (_) ->
                        server:get_timeline(ServerPid, pick_random(UserNames))
                    end,
                    lists:seq(1, NumProcesses div NumThreads)
                ),

                BenchmarkPid ! done

            end) || _ <- lists:seq(1, NumThreads)
            ],

        lists:foreach(fun (_) -> receive done -> ok end end, PIDs)
    end,
    10).    

test_get_users(NumUsers) ->
    NumProcesses = 1,
    NumMessages = 10,
    NumOfSubs = 10,
    {ServerPid, _} = initialize_server_with_params(NumUsers, NumOfSubs, NumMessages),
    run_benchmark("timeline",
        fun () ->
            lists:foreach(fun (_) ->
                server:get_all_users(ServerPid)
            end,
            lists:seq(1, NumProcesses))
        end,
        50).

% single_threaded() ->
%     % {ServerPid, UserNames} = initialize_server(),
%     io:format("Single-threaded benchmark~n"),
%     Start = os:timestamp(),
%     % lists:foreach(fun (_) ->
%         % test,
%         test_send_message_sequential(),
%     % end, lists:seq(1, 10000)),
%     WallClockTime = timer:now_diff(os:timestamp(), Start),
%     io:format("Wall clock time = ~p ms~n", [WallClockTime / 1000.0]),
%     {WallClockTime, ok}.
%{5766257,ok}

test_timeline_with_multi_process(NumThreads, NumProcesses) ->
    {ServerPid, UserNames} = initialize_server(),
    % io:format("test_timeline_with_multi_process benchmark with ~p threads~n", [NumThreads]),
    Start = os:timestamp(),

    run_benchmark("test_timeline_with_multi_process",
     fun () ->
        BenchmarkPid = self(),
        PIDs = [spawn(fun () ->
            lists:foreach(fun (_) ->
                    server:get_timeline(ServerPid, pick_random(UserNames))
                end,
                lists:seq(1, NumProcesses div NumThreads)),
                
                BenchmarkPid ! done

            end) || _ <- lists:seq(1, NumThreads)],

        lists:foreach(fun (_) -> receive done -> ok end end, PIDs)
    end,
     30),
     % calculates the time in microseconds
     WallClockTime = timer:now_diff(os:timestamp(), Start),
     % converting to miliseconds
    %  io:format("Accumulative Wall clock time = ~p ms~n", [WallClockTime / 1000.0]),
     {WallClockTime, ok}.

% JUST for my sake
% I used NumThreads here to simulate the processes (Hypothetical)
% i.e. if there are 1000 messages and 2 threads
% the load will be divided by 1000/2 and each spawned process
% will handle 500 messages
test_send_message_with_multi_process(NumThreads, NumProcesses) ->
    {ServerPid, UserNames} = initialize_server(),
    % io:format("test_send_message_with_multi_process benchmark with ~p threads~n", [NumThreads]),
    Start = os:timestamp(),

    run_benchmark("test_send_message_with_multi_process",
     fun () -> 
        BenchmarkPid = self(),
        % 1. spawns a new process with an anonymous function that send the message, N number of time
        % 2. divide the total number of tasks by N-Threads = TasksPerThread
        % 3. Each spawned function will run anon function this times 👆🏽
        PIDs = [spawn(fun () ->

            lists:foreach(fun (_) ->
                    server:send_message(ServerPid, pick_random(UserNames), "Test")
                end,
            % 3. Spawning the number of N-threads
            % TODO: e3. vary this 50000 to different 
            lists:seq(1, NumProcesses div NumThreads)),

            BenchmarkPid ! done

        end) || _ <- lists:seq(1, NumThreads)],
        % 4. wait for all the tasks to be compete
        % 4. iterate over all the PIDs and listen to "done" message
        lists:foreach(fun (_) -> receive done -> ok end end, PIDs)
    end,
    % 30 x numThreads = totalNumber of processes will be spawned
    % for example: 30 × 3 = 90 (total processes)
     30),
    WallClockTime = timer:now_diff(os:timestamp(), Start),
    % io:format("Accumulative Wall clock time = ~p ms~n", [WallClockTime / 1000.0]),
    {WallClockTime, ok}.






%  -------
% | Best and worst case
%  -------  

%                    Best                                   worst 
% request pattern:  moderate load               | high intensity request (all users fetching large timelines simul...)
% CPU:              Minimal context switching   | too many request, too many context switching
% message queues:   moderate users              | too many processes would increase the message queues


%  -------
% | Sacrificing data
%  -------

% since each user has its own process, 
% there can be consistency or outdated data
% (timeline aggregation)

% Another potential sacrifice would be to unsort the data








% Calculate relative and absolute speedup
% Calculate relative and absolute speedup

% Calculate relative and absolute speedup
% calculate_speedup() ->
%     {_, ParallelTime} = timer:tc(benchmark, test_send_message_parallel, []),
%     {_, SequentialTime} = timer:tc(benchmark, test_send_message_sequential, []),
%     io:format("Parallel Execution Time: ~p microseconds~n", [ParallelTime]),
%     io:format("Sequential Execution Time: ~p microseconds~n", [SequentialTime]),
%     AbsoluteSpeedup = SequentialTime / ParallelTime,
%     io:format("Absolute Speedup: ~p~n", [AbsoluteSpeedup]),
%     RelativeSpeedup = SequentialTime / ParallelTime,
%     io:format("Relative Speedup: ~p~n", [RelativeSpeedup]).


%% Measuring Latency and Throughput
% measure_latency_throughput() ->
%     {ServerPid, UserNames} = initialize_server(),
%     io:format("Measuring latency and throughput~n"),
%     Start = os:timestamp(),
%     Latencies = lists:map(fun (_) ->
%         server:get_timeline(ServerPid, pick_random(UserNames)),
%         End = os:timestamp(),
%         Latency = timer:now_diff(End, Start),
%         {timeline, Latency}
%     end, lists:seq(1, 10000)),
%     Throughput = 10000 / (timer:now_diff(os:timestamp(), Start) / 1000000.0),
%     {Latencies, Throughput}.

% measure_latency_throughput_concurrent(NumThreads) ->
%     {ServerPid, UserNames} = initialize_server(),
%     io:format("Measuring latency and throughput with ~p threads~n", [NumThreads]),
%     Start = os:timestamp(),
%     TasksPerThread = 20 div NumThreads,
%     Pids = [spawn(fun () -> measure_tasks_concurrent(ServerPid, UserNames, TasksPerThread, self()) end) || _ <- lists:seq(1, NumThreads)],
%     lists:foreach(fun (_) -> receive done -> ok end end, Pids),
%     TotalTime = timer:now_diff(os:timestamp(), Start),
%     Throughput = 10000 / (TotalTime / 1000000.0),
%     {TotalTime, Throughput}.

% measure_tasks_concurrent(ServerPid, UserNames, NumTasks, ParentPid) ->
%     lists:foreach(fun (_) ->
%         Start = os:timestamp(),
%         server:get_timeline(ServerPid, pick_random(UserNames)),
%         End = os:timestamp(),
%         Latency = timer:now_diff(End, Start),
%         io:format("Latency = ~p us~n", [Latency])
%     end, lists:seq(1, NumTasks)),
%     ParentPid ! done.

% parallel {38324821,ok}
% sequential {5672934,ok}

% 5672934 / 38324821 = 0,1480224526

% measure_latency_throughput_concurrent_right(NumThreads) ->
%     {ServerPid, UserNames} = initialize_server(),
%     io:format("Measuring latency and throughput with ~p threads~n", [NumThreads]),
%     Start = os:timestamp(),

%     BenchmarkPid = self(),
%     ThisPid = self(),

%     lists:foreach(fun (N) ->
%         io:format("Starting benchmark: ~p~n", [N]),

%         spawn_link(fun () ->

%             % ----

%             PIDs = [spawn(fun () ->

%                 lists:foreach(fun (_) ->
%                         NestedStartTime = os:timestamp(),
%                         server:send_message(ServerPid, pick_random(UserNames), "Test"),
%                         NestedEndTime = os:timestamp(),
%                         Latency = timer:now_diff(NestedEndTime, NestedStartTime)
%                         % io:format("Latency = ~p us~n", [Latency])
%                     end,
%                 % 3. Spawning the number of N-threads
%                 lists:seq(1, 10000 div NumThreads)),

%                 BenchmarkPid ! done

%             end) || _ <- lists:seq(1, NumThreads)],

%             lists:foreach(fun (_) -> receive done -> ok end end, PIDs),

%             % ---- {10191151,981.2434336415976}

%                 ThisPid ! skibidbi

%             end),
%         receive done ->
%             ok
%             end 
%         end, 
%         lists:seq(1, 30)),

    

%     % run_benchmark("test_send_message_with_multi_process",
%     % fun () -> 
%     %    BenchmarkPid = self(),
%     %    % 1. spawns a new process with an anonymous function that send the message, N number of time
%     %    % 2. divide the total number of tasks by N-Threads = TasksPerThread
%     %    % 3. Each spawned function will run anon function this times 👆🏽
%     %    PIDs = [spawn(fun () ->

%     %        lists:foreach(fun (_) ->
%     %                 NestedStartTime = os:timestamp(),
%     %                 server:send_message(ServerPid, pick_random(UserNames), "Test"),
%     %                 NestedEndTime = os:timestamp(),
%     %                 Latency = timer:now_diff(NestedEndTime, NestedStartTime),
%     %                 io:format("Latency = ~p us~n", [Latency])
%     %            end,
%     %        % 3. Spawning the number of N-threads
%     %        lists:seq(1, 10000 div NumThreads)),

%     %        BenchmarkPid ! done

%     %    end) || _ <- lists:seq(1, NumThreads)],
%     %    % 4. wait for all the tasks to be compete
%     %    % 4. iterate over all the PIDs and listen to "done" message
%     %    lists:foreach(fun (_) -> receive done -> ok end end, PIDs)
%     % end,
%     % 50),

%     % Pids = [spawn(fun () -> measure_tasks_concurrent(ServerPid, UserNames, TasksPerThread, self()) end) || _ <- lists:seq(1, NumThreads)],
%     % lists:foreach(fun (_) -> receive done -> ok end end, Pids),
%     TotalTime = timer:now_diff(os:timestamp(), Start),
%     Throughput = 10000 / (TotalTime / 1000000.0),
%     {TotalTime, Throughput}.

% measure_consistency_staleness() ->
%     {ServerPid, UserNames} = initialize_server(),
%     io:format("Measuring consistency and staleness~n"),
%     %% Simulate messages being sent and received
%     lists:foreach(fun (_) ->
%         server:send_message(ServerPid, pick_random(UserNames), "Test Message"),
%         %% Add a delay to simulate real-world conditions
%         timer:sleep(10),
%         server:get_timeline(ServerPid, pick_random(UserNames))
%     end, lists:seq(1, 1000)),
%     %% Measure the number of inconsistencies and staleness
%     Inconsistencies = measure_inconsistencies(ServerPid, UserNames),
%     io:format("Inconsistencies = ~p~n", [Inconsistencies]),
%     ok.

% measure_inconsistencies(ServerPid, UserNames) ->
%     %% Logic to measure inconsistencies
%     %% For simplicity, we will just return a dummy value here
%     5.

% % Function to measure read consistency
% measure_read_consistency(ServerPid, UserNames) ->
%     % Example: Measure time until all users receive a message
%     StartTimestamp = os:timestamp(),
%     lists:foreach(fun (UserName) ->
%         server:send_message(ServerPid, UserName, "Test"),
%         receive
%             {ok, message_sent} ->
%                 io:format("User ~s received message~n", [UserName])
%         end
%     end, UserNames),
%     EndTimestamp = os:timestamp(),
%     io:format("Time to reach all users: ~p ms~n", [timer:now_diff(EndTimestamp, StartTimestamp) / 1000]).
