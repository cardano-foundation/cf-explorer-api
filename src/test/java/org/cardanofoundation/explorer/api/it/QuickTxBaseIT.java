package org.cardanofoundation.explorer.api.it;

import java.util.List;
import java.util.Optional;

import com.bloxbean.cardano.client.api.model.Result;
import com.bloxbean.cardano.client.api.model.Utxo;
import com.bloxbean.cardano.client.backend.api.BackendService;
import com.bloxbean.cardano.client.backend.api.DefaultUtxoSupplier;
import com.bloxbean.cardano.client.backend.blockfrost.common.Constants;
import com.bloxbean.cardano.client.backend.blockfrost.service.BFBackendService;
import com.bloxbean.cardano.client.backend.model.TransactionContent;
import com.bloxbean.cardano.client.util.JsonUtil;

public class QuickTxBaseIT {
  final String bfProjectId = "PROJECT_ID"; // should be set as env variable or system property

  // just support blockfrost for now
  public BackendService getBackendService() {
    return new BFBackendService(Constants.BLOCKFROST_PREPROD_URL, bfProjectId);
  }

  public void waitForTransaction(Result<String> result) {
    try {
      if (result.isSuccessful()) { // Wait for transaction to be mined
        int count = 0;
        while (count < 60) {
          Result<TransactionContent> txnResult =
              getBackendService().getTransactionService().getTransaction(result.getValue());
          if (txnResult.isSuccessful()) {
            System.out.println(JsonUtil.getPrettyJson(txnResult.getValue()));
            break;
          } else {
            System.out.println("Waiting for transaction to be mined ....");
          }

          count++;
          Thread.sleep(2000);
        }
      }
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  protected void checkIfUtxoAvailable(String txHash, String address) {
    Optional<Utxo> utxo = Optional.empty();
    int count = 0;
    while (utxo.isEmpty()) {
      if (count++ >= 20) break;
      List<Utxo> utxos =
          new DefaultUtxoSupplier(getBackendService().getUtxoService()).getAll(address);
      utxo = utxos.stream().filter(u -> u.getTxHash().equals(txHash)).findFirst();
      System.out.println("Try to get new output... txhash: " + txHash);
      try {
        Thread.sleep(1000);
      } catch (Exception e) {
      }
    }
  }
}
