-module(mungo@error).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([is_retriable_error/1, is_not_primary_error/1]).
-export_type([error/0, write_error/0, mongo_server_error/0]).

-type error() :: structure_error |
    authentication_error |
    actor_error |
    {tcp_error, mug:error()} |
    connection_string_error |
    {write_errors, list(write_error())} |
    {server_error, mongo_server_error()}.

-type write_error() :: {write_error, integer(), binary(), bison@bson:value()}.

-type mongo_server_error() :: {ok, binary()} |
    {internal_error, binary()} |
    {bad_value, binary()} |
    {no_such_key, binary()} |
    {graph_contains_cycle, binary()} |
    {host_unreachable, binary()} |
    {host_not_found, binary()} |
    {unknown_error, binary()} |
    {failed_to_parse, binary()} |
    {cannot_mutate_object, binary()} |
    {user_not_found, binary()} |
    {unsupported_format, binary()} |
    {unauthorized, binary()} |
    {type_mismatch, binary()} |
    {overflow, binary()} |
    {invalid_length, binary()} |
    {protocol_error, binary()} |
    {authentication_failed, binary()} |
    {cannot_reuse_object, binary()} |
    {illegal_operation, binary()} |
    {empty_array_operation, binary()} |
    {invalid_bson, binary()} |
    {already_initialized, binary()} |
    {lock_timeout, binary()} |
    {remote_validation_error, binary()} |
    {namespace_not_found, binary()} |
    {index_not_found, binary()} |
    {path_not_viable, binary()} |
    {non_existent_path, binary()} |
    {invalid_path, binary()} |
    {role_not_found, binary()} |
    {roles_not_related, binary()} |
    {privilege_not_found, binary()} |
    {cannot_backfill_array, binary()} |
    {user_modification_failed, binary()} |
    {remote_change_detected, binary()} |
    {file_rename_failed, binary()} |
    {file_not_open, binary()} |
    {file_stream_failed, binary()} |
    {conflicting_update_operators, binary()} |
    {file_already_open, binary()} |
    {log_write_failed, binary()} |
    {cursor_not_found, binary()} |
    {user_data_inconsistent, binary()} |
    {lock_busy, binary()} |
    {no_matching_document, binary()} |
    {namespace_exists, binary()} |
    {invalid_role_modification, binary()} |
    {max_time_ms_expired, binary()} |
    {manual_intervention_required, binary()} |
    {dollar_prefixed_field_name, binary()} |
    {invalid_id_field, binary()} |
    {not_single_value_field, binary()} |
    {invalid_db_ref, binary()} |
    {empty_field_name, binary()} |
    {dotted_field_name, binary()} |
    {role_modification_failed, binary()} |
    {command_not_found, binary()} |
    {shard_key_not_found, binary()} |
    {oplog_operation_unsupported, binary()} |
    {stale_shard_version, binary()} |
    {write_concern_failed, binary()} |
    {multiple_errors_occurred, binary()} |
    {immutable_field, binary()} |
    {cannot_create_index, binary()} |
    {index_already_exists, binary()} |
    {auth_schema_incompatible, binary()} |
    {shard_not_found, binary()} |
    {replica_set_not_found, binary()} |
    {invalid_options, binary()} |
    {invalid_namespace, binary()} |
    {node_not_found, binary()} |
    {write_concern_legacy_ok, binary()} |
    {no_replication_enabled, binary()} |
    {operation_incomplete, binary()} |
    {command_result_schema_violation, binary()} |
    {unknown_repl_write_concern, binary()} |
    {role_data_inconsistent, binary()} |
    {no_match_parse_context, binary()} |
    {no_progress_made, binary()} |
    {remote_results_unavailable, binary()} |
    {index_options_conflict, binary()} |
    {index_key_specs_conflict, binary()} |
    {cannot_split, binary()} |
    {network_timeout, binary()} |
    {callback_canceled, binary()} |
    {shutdown_in_progress, binary()} |
    {secondary_ahead_of_primary, binary()} |
    {invalid_replica_set_config, binary()} |
    {not_yet_initialized, binary()} |
    {not_secondary, binary()} |
    {operation_failed, binary()} |
    {no_projection_found, binary()} |
    {db_path_in_use, binary()} |
    {unsatisfiable_write_concern, binary()} |
    {outdated_client, binary()} |
    {incompatible_audit_metadata, binary()} |
    {new_replica_set_configuration_incompatible, binary()} |
    {node_not_electable, binary()} |
    {incompatible_sharding_metadata, binary()} |
    {distributed_clock_skewed, binary()} |
    {lock_failed, binary()} |
    {inconsistent_replica_set_names, binary()} |
    {configuration_in_progress, binary()} |
    {cannot_initialize_node_with_data, binary()} |
    {not_exact_value_field, binary()} |
    {write_conflict, binary()} |
    {initial_sync_failure, binary()} |
    {initial_sync_oplog_source_missing, binary()} |
    {command_not_supported, binary()} |
    {doc_too_large_for_capped, binary()} |
    {conflicting_operation_in_progress, binary()} |
    {namespace_not_sharded, binary()} |
    {invalid_sync_source, binary()} |
    {oplog_start_missing, binary()} |
    {document_validation_failure, binary()} |
    {not_a_replica_set, binary()} |
    {incompatible_election_protocol, binary()} |
    {command_failed, binary()} |
    {rpc_protocol_negotiation_failed, binary()} |
    {unrecoverable_rollback_error, binary()} |
    {lock_not_found, binary()} |
    {lock_state_change_failed, binary()} |
    {symbol_not_found, binary()} |
    {failed_to_satisfy_read_preference, binary()} |
    {read_concern_majority_not_available_yet, binary()} |
    {stale_term, binary()} |
    {capped_position_lost, binary()} |
    {incompatible_sharding_config_version, binary()} |
    {remote_oplog_stale, binary()} |
    {js_interpreter_failure, binary()} |
    {invalid_ssl_configuration, binary()} |
    {ssl_handshake_failed, binary()} |
    {js_uncatchable_error, binary()} |
    {cursor_in_use, binary()} |
    {incompatible_catalog_manager, binary()} |
    {pooled_connections_dropped, binary()} |
    {exceeded_memory_limit, binary()} |
    {z_lib_error, binary()} |
    {read_concern_majority_not_enabled, binary()} |
    {no_config_primary, binary()} |
    {stale_epoch, binary()} |
    {operation_cannot_be_batched, binary()} |
    {oplog_out_of_order, binary()} |
    {chunk_too_big, binary()} |
    {inconsistent_shard_identity, binary()} |
    {cannot_apply_oplog_while_primary, binary()} |
    {can_repair_to_downgrade, binary()} |
    {must_upgrade, binary()} |
    {duration_overflow, binary()} |
    {max_staleness_out_of_range, binary()} |
    {incompatible_collation_version, binary()} |
    {collection_is_empty, binary()} |
    {zone_still_in_use, binary()} |
    {initial_sync_active, binary()} |
    {view_depth_limit_exceeded, binary()} |
    {command_not_supported_on_view, binary()} |
    {option_not_supported_on_view, binary()} |
    {invalid_pipeline_operator, binary()} |
    {command_on_sharded_view_not_supported_on_mongod, binary()} |
    {too_many_matching_documents, binary()} |
    {cannot_index_parallel_arrays, binary()} |
    {transport_session_closed, binary()} |
    {transport_session_not_found, binary()} |
    {transport_session_unknown, binary()} |
    {query_plan_killed, binary()} |
    {file_open_failed, binary()} |
    {zone_not_found, binary()} |
    {range_overlap_conflict, binary()} |
    {windows_pdh_error, binary()} |
    {bad_perf_counter_path, binary()} |
    {ambiguous_index_key_pattern, binary()} |
    {invalid_view_definition, binary()} |
    {client_metadata_missing_field, binary()} |
    {client_metadata_app_name_too_large, binary()} |
    {client_metadata_document_too_large, binary()} |
    {client_metadata_cannot_be_mutated, binary()} |
    {linearizable_read_concern_error, binary()} |
    {incompatible_server_version, binary()} |
    {primary_stepped_down, binary()} |
    {master_slave_connection_failure, binary()} |
    {fail_point_enabled, binary()} |
    {no_sharding_enabled, binary()} |
    {balancer_interrupted, binary()} |
    {view_pipeline_max_size_exceeded, binary()} |
    {invalid_index_specification_option, binary()} |
    {replica_set_monitor_removed, binary()} |
    {chunk_range_cleanup_pending, binary()} |
    {cannot_build_index_keys, binary()} |
    {network_interface_exceeded_time_limit, binary()} |
    {sharding_state_not_initialized, binary()} |
    {time_proof_mismatch, binary()} |
    {cluster_time_fails_rate_limiter, binary()} |
    {no_such_session, binary()} |
    {invalid_uuid, binary()} |
    {too_many_locks, binary()} |
    {stale_cluster_time, binary()} |
    {cannot_verify_and_sign_logical_time, binary()} |
    {key_not_found, binary()} |
    {incompatible_rollback_algorithm, binary()} |
    {duplicate_session, binary()} |
    {authentication_restriction_unmet, binary()} |
    {database_drop_pending, binary()} |
    {election_in_progress, binary()} |
    {incomplete_transaction_history, binary()} |
    {update_operation_failed, binary()} |
    {ftdc_path_not_set, binary()} |
    {ftdc_path_already_set, binary()} |
    {index_modified, binary()} |
    {close_change_stream, binary()} |
    {illegal_op_msg_flag, binary()} |
    {query_feature_not_allowed, binary()} |
    {transaction_too_old, binary()} |
    {atomicity_failure, binary()} |
    {cannot_implicitly_create_collection, binary()} |
    {session_transfer_incomplete, binary()} |
    {must_downgrade, binary()} |
    {dns_host_not_found, binary()} |
    {dns_protocol_error, binary()} |
    {max_sub_pipeline_depth_exceeded, binary()} |
    {too_many_document_sequences, binary()} |
    {retry_change_stream, binary()} |
    {internal_error_not_supported, binary()} |
    {for_testing_error_extra_info, binary()} |
    {cursor_killed, binary()} |
    {not_implemented, binary()} |
    {snapshot_too_old, binary()} |
    {dns_record_type_mismatch, binary()} |
    {conversion_failure, binary()} |
    {cannot_create_collection, binary()} |
    {incompatible_with_upgraded_server, binary()} |
    {broken_promise, binary()} |
    {snapshot_unavailable, binary()} |
    {producer_consumer_queue_batch_too_large, binary()} |
    {producer_consumer_queue_end_closed, binary()} |
    {stale_db_version, binary()} |
    {stale_chunk_history, binary()} |
    {no_such_transaction, binary()} |
    {reentrancy_not_allowed, binary()} |
    {free_mon_http_in_flight, binary()} |
    {free_mon_http_temporary_failure, binary()} |
    {free_mon_http_permanent_failure, binary()} |
    {transaction_committed, binary()} |
    {transaction_too_large, binary()} |
    {unknown_feature_compatibility_version, binary()} |
    {keyed_executor_retry, binary()} |
    {invalid_resume_token, binary()} |
    {too_many_logical_sessions, binary()} |
    {exceeded_time_limit, binary()} |
    {operation_not_supported_in_transaction, binary()} |
    {too_many_files_open, binary()} |
    {orphaned_range_clean_up_failed, binary()} |
    {fail_point_set_failed, binary()} |
    {prepared_transaction_in_progress, binary()} |
    {cannot_backup, binary()} |
    {data_modified_by_repair, binary()} |
    {repaired_replica_set_node, binary()} |
    {js_interpreter_failure_with_stack, binary()} |
    {migration_conflict, binary()} |
    {producer_consumer_queue_producer_queue_depth_exceeded, binary()} |
    {producer_consumer_queue_consumed, binary()} |
    {exchange_passthrough, binary()} |
    {index_build_aborted, binary()} |
    {alarm_already_fulfilled, binary()} |
    {unsatisfiable_commit_quorum, binary()} |
    {client_disconnect, binary()} |
    {change_stream_fatal_error, binary()} |
    {transaction_coordinator_stepping_down, binary()} |
    {transaction_coordinator_reached_abort_decision, binary()} |
    {would_change_owning_shard, binary()} |
    {for_testing_error_extra_info_with_extra_info_in_namespace, binary()} |
    {index_build_already_in_progress, binary()} |
    {change_stream_history_lost, binary()} |
    {transaction_coordinator_deadline_task_canceled, binary()} |
    {checksum_mismatch, binary()} |
    {wait_for_majority_service_earlier_op_time_available, binary()} |
    {transaction_exceeded_lifetime_limit_seconds, binary()} |
    {no_query_execution_plans, binary()} |
    {query_exceeded_memory_limit_no_disk_use_allowed, binary()} |
    {invalid_seed_list, binary()} |
    {invalid_topology_type, binary()} |
    {invalid_heart_beat_frequency, binary()} |
    {topology_set_name_required, binary()} |
    {hierarchical_acquisition_level_violation, binary()} |
    {invalid_server_type, binary()} |
    {ocsp_certificate_status_revoked, binary()} |
    {range_deletion_abandoned_because_collection_with_uuid_does_not_exist,
        binary()} |
    {data_corruption_detected, binary()} |
    {ocsp_certificate_status_unknown, binary()} |
    {split_horizon_change, binary()} |
    {shard_invalidated_for_targeting, binary()} |
    {read_through_cache_lookup_canceled, binary()} |
    {range_deletion_abandoned_because_task_document_does_not_exist, binary()} |
    {current_config_not_committed_yet, binary()} |
    {exhaust_command_finished, binary()} |
    {periodic_job_is_stopped, binary()} |
    {transaction_coordinator_canceled, binary()} |
    {operation_is_killed_and_delisted, binary()} |
    {resumable_range_deleter_disabled, binary()} |
    {object_is_busy, binary()} |
    {too_stale_to_sync_from_source, binary()} |
    {query_trial_run_completed, binary()} |
    {connection_pool_expired, binary()} |
    {for_testing_optional_error_extra_info, binary()} |
    {move_primary_in_progress, binary()} |
    {tenant_migration_conflict, binary()} |
    {tenant_migration_committed, binary()} |
    {api_version_error, binary()} |
    {api_strict_error, binary()} |
    {api_deprecation_error, binary()} |
    {tenant_migration_aborted, binary()} |
    {oplog_query_min_ts_missing, binary()} |
    {no_such_tenant_migration, binary()} |
    {tenant_migration_access_blocker_shutting_down, binary()} |
    {tenant_migration_in_progress, binary()} |
    {skip_command_execution, binary()} |
    {failed_to_run_with_reply_builder, binary()} |
    {cannot_downgrade, binary()} |
    {service_executor_in_shutdown, binary()} |
    {mechanism_unavailable, binary()} |
    {tenant_migration_forgotten, binary()} |
    {socket_exception, binary()} |
    {cannot_grow_document_in_capped_namespace, binary()} |
    {not_writable_primary, binary()} |
    {bson_object_too_large, binary()} |
    {duplicate_key, binary()} |
    {interrupted_at_shutdown, binary()} |
    {interrupted, binary()} |
    {interrupted_due_to_repl_state_change, binary()} |
    {background_operation_in_progress_for_database, binary()} |
    {background_operation_in_progress_for_namespace, binary()} |
    {merge_stage_no_matching_document, binary()} |
    {database_differ_case, binary()} |
    {stale_config, binary()} |
    {not_primary_no_secondary_ok, binary()} |
    {not_primary_or_secondary, binary()} |
    {out_of_disk_space, binary()} |
    {client_marked_killed, binary()}.

-spec is_retriable_error(mongo_server_error()) -> boolean().
is_retriable_error(Error) ->
    case Error of
        {host_unreachable, _} ->
            true;

        {host_not_found, _} ->
            true;

        {network_timeout, _} ->
            true;

        {shutdown_in_progress, _} ->
            true;

        {primary_stepped_down, _} ->
            true;

        {exceeded_time_limit, _} ->
            true;

        {connection_pool_expired, _} ->
            true;

        {socket_exception, _} ->
            true;

        {not_writable_primary, _} ->
            true;

        {interrupted_at_shutdown, _} ->
            true;

        {interrupted_due_to_repl_state_change, _} ->
            true;

        {not_primary_no_secondary_ok, _} ->
            true;

        {not_primary_or_secondary, _} ->
            true;

        _ ->
            false
    end.

-spec is_not_primary_error(mongo_server_error()) -> boolean().
is_not_primary_error(Error) ->
    case Error of
        {primary_stepped_down, _} ->
            true;

        {not_writable_primary, _} ->
            true;

        {interrupted_due_to_repl_state_change, _} ->
            true;

        {not_primary_no_secondary_ok, _} ->
            true;

        {not_primary_or_secondary, _} ->
            true;

        _ ->
            false
    end.
