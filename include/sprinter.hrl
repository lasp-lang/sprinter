-define(APP, sprinter).

%% State record.
-record(state, {backend,
                is_connected,
                was_connected,
                attempted_nodes,
                peer_service,
                graph,
                tree,
                eredis,
                servers,
                nodes}).
