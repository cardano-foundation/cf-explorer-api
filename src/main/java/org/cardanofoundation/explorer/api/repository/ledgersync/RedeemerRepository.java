package org.cardanofoundation.explorer.api.repository.ledgersync;

import org.cardanofoundation.explorer.api.projection.TxContractProjection;
import org.cardanofoundation.explorer.consumercommon.entity.Redeemer;
import org.cardanofoundation.explorer.consumercommon.entity.Tx;
import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface RedeemerRepository extends JpaRepository<Redeemer, Long> {

  @Query("SELECT re.scriptHash AS scriptHash, txOut.address AS address, re.purpose as purpose,"
      + " rd.bytes as redeemerBytes, re.unitMem as redeemerMem, re.unitSteps as redeemerSteps,"
      + " d.hash as datumHashIn, d.bytes as datumBytesIn, s.bytes as scriptBytes, txOut.id as txOutId,"
      + " txOut.index as utxoIndex, utxo.hash as utxoHash, sa.view as stakeAddress,"
      + " de.id as delegationId, sdr.id as stakeDeregistrationId"
      + " FROM Redeemer re"
      + " INNER JOIN Tx tx ON re.tx = tx"
      + " LEFT JOIN RedeemerData rd ON re.redeemerData = rd"
      + " LEFT JOIN TxIn txIn ON (txIn.redeemer = re and txIn.txInput = tx)"
      + " LEFT JOIN TxOut txOut ON txIn.txOut = txOut.tx AND txIn.txOutIndex = txOut.index"
      + " LEFT JOIN Datum d ON (txOut.dataHash = d.hash OR txOut.inlineDatum = d)"
      + " LEFT JOIN Script s ON re.scriptHash = s.hash"
      + " LEFT JOIN Withdrawal w ON re = w.redeemer"
      + " LEFT JOIN Delegation de ON re = de.redeemer"
      + " LEFT JOIN StakeDeregistration sdr ON re = sdr.redeemer"
      + " LEFT JOIN StakeAddress sa ON (w.addr = sa OR de.address = sa OR sdr.addr = sa)"
      + " LEFT JOIN Tx utxo ON (txOut.tx = utxo)"
      + " WHERE tx = :tx"
      + " ORDER BY re.id")
  List<TxContractProjection> findContractByTx(@Param("tx") Tx tx);

  @Query("SELECT re.scriptHash AS scriptHash, txOut.address AS address, re.purpose as purpose,"
      + " rd.bytes as redeemerBytes, re.unitMem as redeemerMem, re.unitSteps as redeemerSteps,"
      + " d.hash as datumHashIn, d.bytes as datumBytesIn, s.bytes as scriptBytes, txOut.id as txOutId,"
      + " txOut.index as utxoIndex, utxo.hash as utxoHash, sa.view as stakeAddress,"
      + " de.id as delegationId, sdr.id as stakeDeregistrationId"
      + " FROM Redeemer re"
      + " INNER JOIN Tx tx ON re.tx = tx"
      + " LEFT JOIN RedeemerData rd ON re.redeemerData = rd"
      + " LEFT JOIN UnconsumeTxIn uti ON uti.redeemer = re"
      + " LEFT JOIN TxOut txOut ON uti.txOut = txOut.tx AND uti.txOutIndex = txOut.index"
      + " LEFT JOIN Datum d ON (txOut.dataHash = d.hash OR txOut.inlineDatum = d)"
      + " LEFT JOIN Script s ON re.scriptHash = s.hash"
      + " LEFT JOIN Withdrawal w ON re = w.redeemer"
      + " LEFT JOIN Delegation de ON re = de.redeemer"
      + " LEFT JOIN StakeDeregistration sdr ON re = sdr.redeemer"
      + " LEFT JOIN StakeAddress sa ON (w.addr = sa OR de.address = sa OR sdr.addr = sa)"
      + " LEFT JOIN Tx utxo ON (txOut.tx = utxo)"
      + " WHERE tx = :tx"
      + " ORDER BY re.id")
  List<TxContractProjection> findContractByTxFail(@Param("tx") Tx tx);

  @Query(value = "SELECT DISTINCT(tx.id) FROM Tx tx"
      + " JOIN Redeemer r ON r.tx = tx"
      + " WHERE r.scriptHash = :scriptHash")
  Page<Long> findTxIdsInteractWithContract(@Param("scriptHash") String scriptHash, Pageable pageable);
}
