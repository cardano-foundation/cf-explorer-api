CREATE INDEX IF NOT EXISTS idx_epoch_no ON epoch USING btree (no);
CREATE INDEX IF NOT EXISTS idx_reserved_pool_ticker_pool_hash ON reserved_pool_ticker USING btree (pool_hash);
CREATE INDEX IF NOT EXISTS idx_slot_leader_pool_hash_id ON slot_leader USING btree (pool_hash_id);
CREATE INDEX IF NOT EXISTS idx_block_block_no ON block USING btree (block_no);
CREATE INDEX IF NOT EXISTS idx_block_epoch_no ON block USING btree (epoch_no);
CREATE INDEX IF NOT EXISTS idx_block_previous_id ON block USING btree (previous_id);
CREATE INDEX IF NOT EXISTS idx_block_slot_leader_id ON block USING btree (slot_leader_id);
CREATE INDEX IF NOT EXISTS idx_block_slot_no ON block USING btree (slot_no);
CREATE INDEX IF NOT EXISTS idx_block_time ON block USING btree ("time");
CREATE INDEX IF NOT EXISTS idx_cost_model_block_id ON cost_model USING btree (block_id);
CREATE INDEX IF NOT EXISTS idx_epoch_param_block_id ON epoch_param USING btree (block_id);
CREATE INDEX IF NOT EXISTS idx_epoch_param_cost_model_id ON epoch_param USING btree (cost_model_id);
CREATE INDEX IF NOT EXISTS idx_tx_block_id ON tx USING btree (block_id);
CREATE INDEX IF NOT EXISTS idx_tx_metadata_tx_id ON tx_metadata USING btree (tx_id);
CREATE INDEX IF NOT EXISTS idx_collateral_tx_in_tx_out_id ON collateral_tx_in USING btree (tx_out_id);
CREATE INDEX IF NOT EXISTS idx_datum_tx_id ON datum USING btree (tx_id);
CREATE INDEX IF NOT EXISTS idx_extra_key_witness_tx_id ON extra_key_witness USING btree (tx_id);
CREATE INDEX IF NOT EXISTS idx_ma_tx_mint_tx_id ON ma_tx_mint USING btree (tx_id);
CREATE INDEX IF NOT EXISTS idx_param_proposal_cost_model_id ON param_proposal USING btree (cost_model_id);
CREATE INDEX IF NOT EXISTS idx_param_proposal_registered_tx_id ON param_proposal USING btree (registered_tx_id);
CREATE INDEX IF NOT EXISTS idx_pool_metadata_ref_registered_tx_id ON pool_metadata_ref USING btree (registered_tx_id);
CREATE INDEX IF NOT EXISTS idx_pool_offline_data_pmr_id ON pool_offline_data USING btree (pmr_id);
CREATE INDEX IF NOT EXISTS idx_pool_offline_fetch_error_pmr_id ON pool_offline_fetch_error USING btree (pmr_id);
CREATE INDEX IF NOT EXISTS idx_pool_retire_announced_tx_id ON pool_retire USING btree (announced_tx_id);
CREATE INDEX IF NOT EXISTS idx_pool_retire_hash_id ON pool_retire USING btree (hash_id);
CREATE INDEX IF NOT EXISTS redeemer_data_tx_id_idx ON redeemer_data USING btree (tx_id);
CREATE INDEX IF NOT EXISTS reference_tx_in_tx_out_id_idx ON reference_tx_in USING btree (tx_out_id);
CREATE INDEX IF NOT EXISTS idx_script_tx_id ON script USING btree (tx_id);
CREATE INDEX IF NOT EXISTS idx_stake_address_hash_raw ON stake_address USING btree (hash_raw);
CREATE INDEX IF NOT EXISTS idx_stake_address_registered_tx_id ON stake_address USING btree (tx_id);
CREATE INDEX IF NOT EXISTS idx_stake_address_view ON stake_address USING hash (view);
CREATE INDEX IF NOT EXISTS idx_stake_registration_addr_id ON stake_registration USING btree (addr_id);
CREATE INDEX IF NOT EXISTS idx_stake_registration_tx_id ON stake_registration USING btree (tx_id);
CREATE INDEX IF NOT EXISTS idx_treasury_addr_id ON treasury USING btree (addr_id);
CREATE INDEX IF NOT EXISTS idx_treasury_tx_id ON treasury USING btree (tx_id);
CREATE INDEX IF NOT EXISTS idx_tx_out_address ON tx_out USING hash (address);
CREATE INDEX IF NOT EXISTS idx_tx_out_payment_cred ON tx_out USING btree (payment_cred);
CREATE INDEX IF NOT EXISTS idx_tx_out_stake_address_id ON tx_out USING btree (stake_address_id);
CREATE INDEX IF NOT EXISTS idx_tx_out_tx_id ON tx_out USING btree (tx_id);
CREATE INDEX IF NOT EXISTS tx_out_inline_datum_id_idx ON tx_out USING btree (inline_datum_id);
CREATE INDEX IF NOT EXISTS tx_out_reference_script_id_idx ON tx_out USING btree (reference_script_id);
CREATE INDEX IF NOT EXISTS collateral_tx_out_inline_datum_id_idx ON collateral_tx_out USING btree (inline_datum_id);
CREATE INDEX IF NOT EXISTS collateral_tx_out_reference_script_id_idx ON collateral_tx_out USING btree (reference_script_id);
CREATE INDEX IF NOT EXISTS collateral_tx_out_stake_address_id_idx ON collateral_tx_out USING btree (stake_address_id);
CREATE INDEX IF NOT EXISTS idx_epoch_stake_addr_id ON epoch_stake USING btree (addr_id);
CREATE INDEX IF NOT EXISTS idx_epoch_stake_epoch_no ON epoch_stake USING btree (epoch_no);
CREATE INDEX IF NOT EXISTS idx_epoch_stake_pool_id ON epoch_stake USING btree (pool_id);
CREATE INDEX IF NOT EXISTS idx_ma_tx_out_tx_out_id ON ma_tx_out USING btree (tx_out_id);
CREATE INDEX IF NOT EXISTS idx_pool_update_active_epoch_no ON pool_update USING btree (active_epoch_no);
CREATE INDEX IF NOT EXISTS idx_pool_update_hash_id ON pool_update USING btree (hash_id);
CREATE INDEX IF NOT EXISTS idx_pool_update_meta_id ON pool_update USING btree (meta_id);
CREATE INDEX IF NOT EXISTS idx_pool_update_registered_tx_id ON pool_update USING btree (registered_tx_id);
CREATE INDEX IF NOT EXISTS idx_pool_update_reward_addr ON pool_update USING btree (reward_addr_id);
CREATE INDEX IF NOT EXISTS redeemer_redeemer_data_id_idx ON redeemer USING btree (redeemer_data_id);
CREATE INDEX IF NOT EXISTS idx_reserve_addr_id ON reserve USING btree (addr_id);
CREATE INDEX IF NOT EXISTS idx_reserve_tx_id ON reserve USING btree (tx_id);
CREATE INDEX IF NOT EXISTS idx_reward_addr_id ON reward USING btree (addr_id);
CREATE INDEX IF NOT EXISTS idx_reward_earned_epoch ON reward USING btree (earned_epoch);
CREATE INDEX IF NOT EXISTS idx_reward_pool_id ON reward USING btree (pool_id);
CREATE INDEX IF NOT EXISTS idx_stake_deregistration_addr_id ON stake_deregistration USING btree (addr_id);
CREATE INDEX IF NOT EXISTS idx_stake_deregistration_redeemer_id ON stake_deregistration USING btree (redeemer_id);
CREATE INDEX IF NOT EXISTS idx_stake_deregistration_tx_id ON stake_deregistration USING btree (tx_id);
CREATE INDEX IF NOT EXISTS idx_tx_in_redeemer_id ON tx_in USING btree (redeemer_id);
CREATE INDEX IF NOT EXISTS idx_tx_in_source_tx ON tx_in USING btree (tx_in_id);
CREATE INDEX IF NOT EXISTS idx_tx_in_tx_in_id ON tx_in USING btree (tx_in_id);
CREATE INDEX IF NOT EXISTS idx_tx_in_tx_out_id ON tx_in USING btree (tx_out_id);
CREATE INDEX IF NOT EXISTS idx_withdrawal_addr_id ON withdrawal USING btree (addr_id);
CREATE INDEX IF NOT EXISTS idx_withdrawal_redeemer_id ON withdrawal USING btree (redeemer_id);
CREATE INDEX IF NOT EXISTS idx_withdrawal_tx_id ON withdrawal USING btree (tx_id);
CREATE INDEX IF NOT EXISTS idx_delegation_active_epoch_no ON delegation USING btree (active_epoch_no);
CREATE INDEX IF NOT EXISTS idx_delegation_addr_id ON delegation USING btree (addr_id);
CREATE INDEX IF NOT EXISTS idx_delegation_pool_hash_id ON delegation USING btree (pool_hash_id);
CREATE INDEX IF NOT EXISTS idx_delegation_redeemer_id ON delegation USING btree (redeemer_id);
CREATE INDEX IF NOT EXISTS idx_delegation_tx_id ON delegation USING btree (tx_id);
CREATE INDEX IF NOT EXISTS pool_owner_pool_update_id_idx ON pool_owner USING btree (pool_update_id);
CREATE INDEX IF NOT EXISTS idx_pool_relay_update_id ON pool_relay USING btree (update_id);

CREATE INDEX IF NOT EXISTS idx_address_stake_address_id ON address USING btree (stake_address_id);
CREATE INDEX IF NOT EXISTS idx_address_address ON address USING hash (address);
CREATE INDEX IF NOT EXISTS idx_address_tx_balance_time ON address_tx_balance USING btree ("time");
CREATE INDEX IF NOT EXISTS idx_address_tx_balance_tx_id ON address_tx_balance USING btree (tx_id);
CREATE INDEX IF NOT EXISTS idx_address_tx_balance_address ON address_tx_balance USING hash (address);
CREATE INDEX IF NOT EXISTS idx_address_token_address ON address_token USING hash (address);
CREATE INDEX IF NOT EXISTS idx_address_token_tx_id ON address_token USING btree (tx_id);
CREATE INDEX IF NOT EXISTS idx_address_token_ident ON address_token USING btree (ident)

