package org.cardanofoundation.explorer.api.it;

import static com.bloxbean.cardano.client.common.ADAConversionUtil.adaToLovelace;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.math.BigInteger;

import com.bloxbean.cardano.client.account.Account;
import com.bloxbean.cardano.client.api.model.Result;
import com.bloxbean.cardano.client.backend.api.BackendService;
import com.bloxbean.cardano.client.backend.blockfrost.common.Constants;
import com.bloxbean.cardano.client.backend.blockfrost.service.BFBackendService;
import com.bloxbean.cardano.client.common.model.Networks;
import com.bloxbean.cardano.client.crypto.cip1852.DerivationPath;
import com.bloxbean.cardano.client.function.helper.SignerProviders;
import com.bloxbean.cardano.client.governance.DRepId;
import com.bloxbean.cardano.client.quicktx.QuickTxBuilder;
import com.bloxbean.cardano.client.quicktx.Tx;
import com.bloxbean.cardano.client.spec.UnitInterval;
import com.bloxbean.cardano.client.transaction.spec.ProtocolParamUpdate;
import com.bloxbean.cardano.client.transaction.spec.ProtocolVersion;
import com.bloxbean.cardano.client.transaction.spec.Withdrawal;
import com.bloxbean.cardano.client.transaction.spec.governance.Anchor;
import com.bloxbean.cardano.client.transaction.spec.governance.Constitution;
import com.bloxbean.cardano.client.transaction.spec.governance.DRep;
import com.bloxbean.cardano.client.transaction.spec.governance.DRepType;
import com.bloxbean.cardano.client.transaction.spec.governance.Vote;
import com.bloxbean.cardano.client.transaction.spec.governance.Voter;
import com.bloxbean.cardano.client.transaction.spec.governance.VoterType;
import com.bloxbean.cardano.client.transaction.spec.governance.actions.GovActionId;
import com.bloxbean.cardano.client.transaction.spec.governance.actions.HardForkInitiationAction;
import com.bloxbean.cardano.client.transaction.spec.governance.actions.InfoAction;
import com.bloxbean.cardano.client.transaction.spec.governance.actions.NewConstitution;
import com.bloxbean.cardano.client.transaction.spec.governance.actions.NoConfidence;
import com.bloxbean.cardano.client.transaction.spec.governance.actions.ParameterChangeAction;
import com.bloxbean.cardano.client.transaction.spec.governance.actions.TreasuryWithdrawalsAction;
import com.bloxbean.cardano.client.transaction.spec.governance.actions.UpdateCommittee;
import com.bloxbean.cardano.client.util.HexUtil;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

@Disabled
public class GovernanceTxIT extends QuickTxBaseIT {
  BackendService backendService;
  Account sender1; // dRep1
  Account sender2; // delegate to dRep1

  String sender1Addr;
  String sender2Addr;

  Account sender3; // dRep2
  Account sender4; // delegate to dRep2

  String sender3Addr;
  String sender4Addr;

  Account sender5; // dRep3
  Account sender6; // delegate to dRep3

  String sender5Addr;
  String sender6Addr;

  QuickTxBuilder quickTxBuilder;

  /**
   * @return
   */
  @Override
  public BackendService getBackendService() {
    // create project in blockfrost https://blockfrost.io/dashboard and set the project id
    final String bfProjectId = "sanchonetnitMGu4JM7WRigrqZCl3SEcHwxdTauVD";
    return new BFBackendService(Constants.BLOCKFROST_SANCHONET_URL, bfProjectId);
  }

  @BeforeEach
  void setup() {
    backendService = getBackendService();
    quickTxBuilder = new QuickTxBuilder(backendService);

    // mnemonic of wallet account
    // create a wallet account using Eternal wallet extension and use the mnemonic phrase
    // note: this mnemonic contains 10 internal account
    String mnemonic =
        "success view van broccoli robust shock erupt veteran develop visit tray visa mosquito grab milk then route erode kiss urge raven change movie quantum";

    // addr_test1qp2ve72ha2w5gsn32quej6zhwrrukrw4zn02q720qsrsqvndwllp4c92zqm7q4r90uwern2s8m7s2w8j2ensf53jcu3s837cpw
    // sender 1 would be registered as a Drep
    sender1 =
        new Account(Networks.testnet(), mnemonic, createExternalAddressDerivationPathForAccount(0));
    sender1Addr = sender1.baseAddress();

    // sender2 would be delegated to DRep(sender1) to join the voting in governance action
    // addr_test1qp24lety7kyedfk572qaaca85czn6rsq83cynmq3uysmp7n6ae64kf6et05fhxyfz7nmx5ex02tr20rta8t6pkckdhmsqaffrm
    sender2 =
        new Account(Networks.testnet(), mnemonic, createExternalAddressDerivationPathForAccount(1));
    sender2Addr = sender2.baseAddress();

    // sender3 would be registered as a DRep
    // addr_test1qrc4u009ktty382ad4jx23zjqkdl92hkqs8yp2lc0a48vsj7vzflvq7dnuu6laql5czy02hqqfp9sfzsmazz5uafgm8qu7lwlk
    sender3 =
        new Account(Networks.testnet(), mnemonic, createExternalAddressDerivationPathForAccount(2));
    sender3Addr = sender3.baseAddress();

    // sender4 would be delegated to DRep(sender3) to join the voting in governance action
    // addr_test1qpk8nmcmx4uhr344x8phdug2xhfqsqc5urwhztg6rct6u4eke5qlq9pxgje3s52p7evzqjjpnw0twpjzjna3qncktpvs3hgk7l
    sender4 =
        new Account(Networks.testnet(), mnemonic, createExternalAddressDerivationPathForAccount(3));
    sender4Addr = sender4.baseAddress();

    // sender5 would be registered as a Drep
    // addr_test1qpul429nud8uljxfnavcg2jsftj2k75urnuj8zr2qshrq6e6h3sjmjgu606x5uskuvurndp7n8ycqhfcpwq76lm3e0dq2sf56g
    sender5 =
        new Account(Networks.testnet(), mnemonic, createExternalAddressDerivationPathForAccount(4));
    sender5Addr = sender5.baseAddress();

    // sender6 would be delegated to DRep(sender5) to join the voting in governance action
    // addr_test1qr64x4pv7lmukhkjy8tv45h6e0ruk7pwm9csefgt5072e7dvzku8vrpvcysgzz6kgd60xk5au024kzgk5avl8tt2c69s0mfe0w
    sender6 =
        new Account(Networks.testnet(), mnemonic, createExternalAddressDerivationPathForAccount(5));
    sender6Addr = sender6.baseAddress();
  }

  @Test
  void registerDrep() {
    var anchor =
        new Anchor(
            "https://shorturl.at/vBIJ8",
            HexUtil.decodeHexString(
                "6dd65423ea0754ddf8a1a142dfc8152797b6fb4a4cd174a0cd3028f681a0c755"));
    registerDrep(sender1, anchor);
    //    registerDrep(sender3, anchor);
    //    registerDrep(sender5, anchor);
  }

  @Test
  void voteDelegation() {
    stakeAddressRegistration(sender1, sender1.baseAddress());
    voteDelegation(sender1, sender1);
    stakeAddressRegistration(sender1, sender2.baseAddress());
    voteDelegation(sender1, sender2);

    //    stakeAddressRegistration(sender3, sender3.baseAddress());
    //    voteDelegation(sender3, sender3);
    //    stakeAddressRegistration(sender3, sender4.baseAddress());
    //    voteDelegation(sender3, sender4);
    //
    //    stakeAddressRegistration(sender5, sender5.baseAddress());
    //    voteDelegation(sender5, sender5);
    //    stakeAddressRegistration(sender5, sender6.baseAddress());
    //    voteDelegation(sender5, sender6);
  }

  void registerDrep(Account drep, Anchor anchor) {
    QuickTxBuilder quickTxBuilder = new QuickTxBuilder(backendService);

    Tx drepRegTx = new Tx().registerDRep(drep, anchor).from(drep.baseAddress());

    Result<String> result =
        quickTxBuilder
            .compose(drepRegTx)
            .withSigner(SignerProviders.signerFrom(drep))
            .withSigner(SignerProviders.signerFrom(drep.drepHdKeyPair()))
            .complete();

    System.out.println("DRepId : " + drep.drepId());

    System.out.println(result);
    assertTrue(result.isSuccessful());
    waitForTransaction(result);

    checkIfUtxoAvailable(result.getValue(), drep.baseAddress());
  }

  void voteDelegation(Account dRepAccount, Account delegateAccount) {
    QuickTxBuilder quickTxBuilder = new QuickTxBuilder(backendService);

    DRep drep = DRepId.toDrep(dRepAccount.drepId(), DRepType.ADDR_KEYHASH);
    System.out.println("Drep : " + dRepAccount.drepId());

    Tx tx =
        new Tx()
            .delegateVotingPowerTo(delegateAccount.baseAddress(), drep)
            .from(dRepAccount.baseAddress());

    Result<String> result =
        quickTxBuilder
            .compose(tx)
            .withSigner(SignerProviders.stakeKeySignerFrom(delegateAccount))
            .withSigner(SignerProviders.signerFrom(dRepAccount))
            .withTxInspector(
                transaction -> {
                  System.out.println(transaction);
                })
            .completeAndWait(s -> System.out.println(s));

    System.out.println(result);
    assertTrue(result.isSuccessful());
    checkIfUtxoAvailable(result.getValue(), dRepAccount.baseAddress());
  }

  void stakeAddressRegistration(Account account, String addressToRegister) {
    QuickTxBuilder quickTxBuilder = new QuickTxBuilder(backendService);
    Tx tx = new Tx().registerStakeAddress(addressToRegister).from(account.baseAddress());

    System.out.println("Registering stake address for address: " + addressToRegister);
    Result<String> result =
        quickTxBuilder
            .compose(tx)
            .withSigner(SignerProviders.signerFrom(account))
            .completeAndWait(msg -> System.out.println(msg));

    assertTrue(result.isSuccessful());
    checkIfUtxoAvailable(result.getValue(), account.baseAddress());
  }

  @Test
  void deRegisterDrep() {
    deRegisterDrep(sender1);
  }

  void deRegisterDrep(Account drep) {
    QuickTxBuilder quickTxBuilder = new QuickTxBuilder(backendService);
    Tx tx = new Tx().unregisterDRep(drep.drepCredential()).from(drep.baseAddress());

    Result<String> result =
        quickTxBuilder
            .compose(tx)
            .withSigner(SignerProviders.drepKeySignerFrom(drep))
            .withSigner(SignerProviders.signerFrom(drep))
            .complete();

    System.out.println(result);
    assertTrue(result.isSuccessful());
    waitForTransaction(result);

    checkIfUtxoAvailable(result.getValue(), drep.baseAddress());
  }

  @Test
  void updateDrep() {
    var anchor =
        new Anchor(
            "https://shorturl.at/vBIJ8",
            HexUtil.decodeHexString(
                "6dd65423ea0754ddf8a1a142dfc8152797b6fb4a4cd174a0cd3028f681a0c755"));

    updateDrep(sender1, anchor);
  }

  void updateDrep(Account drep, Anchor anchor) {
    QuickTxBuilder quickTxBuilder = new QuickTxBuilder(backendService);

    Tx drepRegTx = new Tx().updateDRep(drep.drepCredential(), anchor).from(drep.baseAddress());

    Result<String> result =
        quickTxBuilder
            .compose(drepRegTx)
            .withSigner(SignerProviders.drepKeySignerFrom(drep))
            .withSigner(SignerProviders.signerFrom(drep))
            .complete();

    System.out.println("DRepId : " + drep.drepId());
    System.out.println(result);
    assertTrue(result.isSuccessful());
    waitForTransaction(result);

    checkIfUtxoAvailable(result.getValue(), drep.baseAddress());
  }

  @Test
  void createProposal_infoAction() {
    QuickTxBuilder quickTxBuilder = new QuickTxBuilder(backendService);

    var govAction = new InfoAction();
    var anchor =
        new Anchor(
            "https://shorturl.at/vBIJ8",
            HexUtil.decodeHexString(
                "6dd65423ea0754ddf8a1a142dfc8152797b6fb4a4cd174a0cd3028f681a0c755"));

    Tx tx =
        new Tx()
            .createProposal(govAction, adaToLovelace(50000), sender1.stakeAddress(), anchor)
            .from(sender1Addr);

    Result<String> result =
        quickTxBuilder
            .compose(tx)
            .withSigner(SignerProviders.drepKeySignerFrom(sender1))
            .withSigner(SignerProviders.signerFrom(sender1))
            .completeAndWait(s -> System.out.println(s));

    System.out.println(result);
    assertTrue(result.isSuccessful());
    checkIfUtxoAvailable(result.getValue(), sender1Addr);
  }

  @Test
  void createProposal_treasuryWithdrawalAction() {
    QuickTxBuilder quickTxBuilder = new QuickTxBuilder(backendService);

    var treasuryWithdrawalsAction = new TreasuryWithdrawalsAction();
    treasuryWithdrawalsAction.addWithdrawal(
        new Withdrawal(sender1.stakeAddress(), adaToLovelace(50000)));
    var anchor =
        new Anchor(
            "https://shorturl.at/vBIJ8",
            HexUtil.decodeHexString(
                "6dd65423ea0754ddf8a1a142dfc8152797b6fb4a4cd174a0cd3028f681a0c755"));

    Tx tx =
        new Tx()
            .createProposal(
                treasuryWithdrawalsAction, adaToLovelace(50000), sender1.stakeAddress(), anchor)
            .from(sender1Addr);

    Result<String> result =
        quickTxBuilder
            .compose(tx)
            .withSigner(SignerProviders.drepKeySignerFrom(sender1))
            .withSigner(SignerProviders.signerFrom(sender1))
            .completeAndWait(s -> System.out.println(s));

    System.out.println(result);
    assertTrue(result.isSuccessful());
    checkIfUtxoAvailable(result.getValue(), sender1Addr);
  }

  @Test
  void createProposal_parameterChangeAction() {
    QuickTxBuilder quickTxBuilder = new QuickTxBuilder(backendService);

    var parameterChange = new ParameterChangeAction();
//    parameterChange.setPrevGovActionId(
//        new GovActionId("950d4b364840a27afeba929324d51dec0fac80b00cf7ca37905de08e3eae5ca6", 0));
    parameterChange.setProtocolParamUpdate(
        ProtocolParamUpdate.builder().keyDeposit(new BigInteger("50000000000")).build());

    var anchor =
        new Anchor(
            "https://shorturl.at/vBIJ8",
            HexUtil.decodeHexString(
                "6dd65423ea0754ddf8a1a142dfc8152797b6fb4a4cd174a0cd3028f681a0c755"));

    Tx tx =
        new Tx()
            .createProposal(parameterChange, new BigInteger("50000000000"), sender1.stakeAddress(), anchor)
            .from(sender1Addr);

    Result<String> result =
        quickTxBuilder
            .compose(tx)
            .withSigner(SignerProviders.drepKeySignerFrom(sender1))
            .withSigner(SignerProviders.signerFrom(sender1))
            .completeAndWait(s -> System.out.println(s));

    System.out.println(result);
    assertTrue(result.isSuccessful());
    checkIfUtxoAvailable(result.getValue(), sender1Addr);
  }

  @Test
  void createProposal_newConstitutionAction() {
    QuickTxBuilder quickTxBuilder = new QuickTxBuilder(backendService);

    var anchor =
        new Anchor(
            "https://shorturl.at/vBIJ8",
            HexUtil.decodeHexString(
                "6dd65423ea0754ddf8a1a142dfc8152797b6fb4a4cd174a0cd3028f681a0c755"));
    var govAction = new NewConstitution();
//    govAction.setPrevGovActionId(
//        new GovActionId("597686b8c917ba2c74cd0018f3fb325bddf0f1fe747038170c41373376c03b5c", 0));
    govAction.setConstitution(Constitution.builder().anchor(anchor).build());

    Tx tx =
        new Tx()
            .createProposal(govAction, new BigInteger("50000000000"), sender1.stakeAddress(), anchor)
            .from(sender1Addr);

    Result<String> result =
        quickTxBuilder
            .compose(tx)
            .withSigner(SignerProviders.drepKeySignerFrom(sender1))
            .withSigner(SignerProviders.signerFrom(sender1))
            .completeAndWait(s -> System.out.println(s));

    System.out.println(result);
    assertTrue(result.isSuccessful());
    checkIfUtxoAvailable(result.getValue(), sender1Addr);
  }

  @Test
  void createProposal_noConfidence() {
    QuickTxBuilder quickTxBuilder = new QuickTxBuilder(backendService);

    var noConfidence = new NoConfidence();
    // if there is no previous action id, then set it to null
    //    noConfidence.setPrevGovActionId(
    //        new GovActionId("e86050ac376fc4df7c76635f648c963f44702e13beb81a5c9971a418013c74dc",
    // 0));

    var anchor =
        new Anchor(
            "https://shorturl.at/vBIJ8",
            HexUtil.decodeHexString(
                "6dd65423ea0754ddf8a1a142dfc8152797b6fb4a4cd174a0cd3028f681a0c755"));

    Tx tx =
        new Tx()
            .createProposal(noConfidence, adaToLovelace(50000), sender1.stakeAddress(), anchor)
            .from(sender1Addr);

    Result<String> result =
        quickTxBuilder
            .compose(tx)
            .withSigner(SignerProviders.drepKeySignerFrom(sender1))
            .withSigner(SignerProviders.signerFrom(sender1))
            .completeAndWait(s -> System.out.println(s));

    System.out.println(result);
    assertTrue(result.isSuccessful());
    checkIfUtxoAvailable(result.getValue(), sender1Addr);
  }

  @Test
  void createProposal_updateCommittee() {
    QuickTxBuilder quickTxBuilder = new QuickTxBuilder(backendService);

    var updateCommittee = new UpdateCommittee();

    // if there is no previous action id, then set it to null
    //    updateCommittee.setPrevGovActionId(
    //        new GovActionId("b3ce0371310a07a797657d19453d953bb352b6841c2f5c5e0bd2557189ef5c3a",
    // 0));

    updateCommittee.setQuorumThreshold(
        new UnitInterval(BigInteger.valueOf(1), BigInteger.valueOf(3)));

    var anchor =
        new Anchor(
            "https://shorturl.at/vBIJ8",
            HexUtil.decodeHexString(
                "6dd65423ea0754ddf8a1a142dfc8152797b6fb4a4cd174a0cd3028f681a0c755"));

    Tx tx =
        new Tx()
            .createProposal(updateCommittee, adaToLovelace(50000), sender1.stakeAddress(), anchor)
            .from(sender1Addr);

    Result<String> result =
        quickTxBuilder
            .compose(tx)
            .withSigner(SignerProviders.drepKeySignerFrom(sender1))
            .withSigner(SignerProviders.signerFrom(sender1))
            .completeAndWait(s -> System.out.println(s));

    System.out.println(result);
    assertTrue(result.isSuccessful());
    checkIfUtxoAvailable(result.getValue(), sender1Addr);
  }

  @Test
  void createProposal_hardforkInitiation() {
    QuickTxBuilder quickTxBuilder = new QuickTxBuilder(backendService);

    var hardforkInitiation = new HardForkInitiationAction();

    // if there is no previous action id, then set it to null
    //    hardforkInitiation.setPrevGovActionId(
    //        new GovActionId("416f7f01c548a85546aa5bbd155b34bb2802df68e08db4e843ef6da764cd8f7e",
    // 0));
    hardforkInitiation.setProtocolVersion(new ProtocolVersion(10, 0));

    var anchor =
        new Anchor(
            "https://shorturl.at/vBIJ8",
            HexUtil.decodeHexString(
                "6dd65423ea0754ddf8a1a142dfc8152797b6fb4a4cd174a0cd3028f681a0c755"));

    Tx tx =
        new Tx()
            .createProposal(hardforkInitiation, adaToLovelace(50000), sender1.stakeAddress(), anchor)
            .from(sender1Addr);

    Result<String> result =
        quickTxBuilder
            .compose(tx)
            .withSigner(SignerProviders.drepKeySignerFrom(sender1))
            .withSigner(SignerProviders.signerFrom(sender1))
            .completeAndWait(s -> System.out.println(s));

    System.out.println(result);
    assertTrue(result.isSuccessful());
    checkIfUtxoAvailable(result.getValue(), sender1Addr);
  }

  @Test
  void createVote() {
    var anchor =
        new Anchor(
            "https://shorturl.at/vBIJ8",
            HexUtil.decodeHexString(
                "6dd65423ea0754ddf8a1a142dfc8152797b6fb4a4cd174a0cd3028f681a0c755"));
    var govActionId =
        new GovActionId("c99298a017b72988a00be57dd1aab195eaa263f654bb58e8490d685843fba861", 0);

    createVote(govActionId, Vote.YES, anchor, sender1);
    createVote(govActionId, Vote.NO, anchor, sender3);
    createVote(govActionId, Vote.ABSTAIN, anchor, sender5);
  }

  void createVote(GovActionId govActionId, Vote vote, Anchor anchor, Account drep) {
    QuickTxBuilder quickTxBuilder = new QuickTxBuilder(backendService);

    var voter = new Voter(VoterType.DREP_KEY_HASH, drep.drepCredential());
    Tx tx = new Tx().createVote(voter, govActionId, vote, anchor).from(drep.baseAddress());

    Result<String> result =
        quickTxBuilder
            .compose(tx)
            .withSigner(SignerProviders.drepKeySignerFrom(drep))
            .withSigner(SignerProviders.signerFrom(drep))
            .completeAndWait(s -> System.out.println(s));

    System.out.println(result);
    assertTrue(result.isSuccessful());
    checkIfUtxoAvailable(result.getValue(), drep.baseAddress());
  }

  private DerivationPath createExternalAddressDerivationPathForAccount(int account) {
    return DerivationPath.createExternalAddressDerivationPathForAccount(account);
  }
}
