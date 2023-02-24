package com.cardano.explorer.service.impl;

import com.cardano.explorer.common.enumeration.TxStatus;
import com.cardano.explorer.exception.BusinessCode;
import com.cardano.explorer.mapper.AssetMetadataMapper;
import com.cardano.explorer.mapper.DelegationMapper;
import com.cardano.explorer.mapper.MaTxMintMapper;
import com.cardano.explorer.mapper.TxMapper;
import com.cardano.explorer.mapper.TxOutMapper;
import com.cardano.explorer.mapper.WithdrawalMapper;
import com.cardano.explorer.model.request.TxFilterRequest;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.TxFilterResponse;
import com.cardano.explorer.model.response.TxResponse;
import com.cardano.explorer.model.response.dashboard.TxGraph;
import com.cardano.explorer.model.response.dashboard.TxSummary;
import com.cardano.explorer.model.response.tx.CollateralResponse;
import com.cardano.explorer.model.response.tx.ContractResponse;
import com.cardano.explorer.model.response.tx.SummaryResponse;
import com.cardano.explorer.model.response.tx.TxMintingResponse;
import com.cardano.explorer.model.response.tx.TxOutResponse;
import com.cardano.explorer.model.response.tx.UTxOResponse;
import com.cardano.explorer.model.response.tx.WithdrawalResponse;
import com.cardano.explorer.projection.AddressInputOutputProjection;
import com.cardano.explorer.projection.CollateralInputOutputProjection;
import com.cardano.explorer.projection.TxContractProjection;
import com.cardano.explorer.projection.TxGraphProjection;
import com.cardano.explorer.projection.TxIOProjection;
import com.cardano.explorer.repository.AddressTokenRepository;
import com.cardano.explorer.repository.AddressTxBalanceRepository;
import com.cardano.explorer.repository.AssetMetadataRepository;
import com.cardano.explorer.repository.BlockRepository;
import com.cardano.explorer.repository.CollateralTxInRepository;
import com.cardano.explorer.repository.DelegationRepository;
import com.cardano.explorer.repository.EpochRepository;
import com.cardano.explorer.repository.MaTxMintRepository;
import com.cardano.explorer.repository.MultiAssetRepository;
import com.cardano.explorer.repository.RedeemerRepository;
import com.cardano.explorer.repository.TxOutRepository;
import com.cardano.explorer.repository.TxRepository;
import com.cardano.explorer.repository.WithdrawalRepository;
import com.cardano.explorer.service.TxService;
import com.cardano.explorer.specification.BlockSpecification;
import com.cardano.explorer.specification.TxSpecification;
import com.cardano.explorer.util.TimeUtil;
import com.sotatek.cardano.common.entity.AssetMetadata;
import com.sotatek.cardano.common.entity.BaseEntity_;
import com.sotatek.cardano.common.entity.Block;
import com.sotatek.cardano.common.entity.Delegation;
import com.sotatek.cardano.common.entity.Epoch;
import com.sotatek.cardano.common.entity.MaTxMint;
import com.sotatek.cardano.common.entity.MultiAsset;
import com.sotatek.cardano.common.entity.Tx;
import com.sotatek.cardano.common.entity.Withdrawal;
import com.sotatek.cardano.common.enumeration.ScriptPurposeType;
import com.sotatek.cardanocommonapi.exceptions.BusinessException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.sql.Timestamp;
import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

@Service
@RequiredArgsConstructor
public class TxServiceImpl implements TxService {

  private final TxRepository txRepository;
  private final TxOutRepository txOutRepository;
  private final BlockRepository blockRepository;
  private final TxMapper txMapper;
  private final TxOutMapper txOutMapper;
  private final TxSpecification txSpecification;
  private final RedeemerRepository redeemerRepository;
  private final EpochRepository epochRepository;
  private final CollateralTxInRepository collateralTxInRepository;
  private final WithdrawalRepository withdrawalRepository;
  private final WithdrawalMapper withdrawalMapper;
  private final DelegationRepository delegationRepository;
  private final DelegationMapper delegationMapper;
  private final MaTxMintRepository maTxMintRepository;
  private final MaTxMintMapper maTxMintMapper;
  private final AddressTxBalanceRepository addressTxBalanceRepository;
  private final MultiAssetRepository multiAssetRepository;
  private final AddressTokenRepository addressTokenRepository;
  private final AssetMetadataRepository assetMetadataRepository;
  private final AssetMetadataMapper assetMetadataMapper;
  private static final int SUMMARY_SIZE = 4;
  private static final long MINUS_DAYS = 15;

  @Override
  @Transactional(readOnly = true)
  public List<TxSummary> findLatestTxSummary() {
    Page<Long> txIds = txRepository.findLatestTxId(
        PageRequest.of(BigInteger.ZERO.intValue(),
            SUMMARY_SIZE,
            Sort.by(BaseEntity_.ID).descending()));

    if (txIds.isEmpty()) {
      return Collections.emptyList();
    }

    List<TxSummary> summaries = new ArrayList<>();
    List<TxIOProjection> txs = txRepository.findLatestTxIO(txIds.toList());

    txs.forEach(tx -> {

      Optional<TxSummary> searchedSummary = summaries.stream().filter(summary ->
          summary.getHash().equals(tx.getHash())
      ).findFirst();

      if (searchedSummary.isEmpty()) {

        final var from = new ArrayList<String>();
        from.add(tx.getFromAddress());

        final var to = new ArrayList<String>();
        to.add(tx.getToAddress());

        final TxSummary summary = TxSummary.builder()
            .blockNo(tx.getBlockNo())
            .hash(tx.getHash())
            .amount(tx.getAmount().doubleValue())
            .fromAddress(from)
            .toAddress(to)
            .build();

        summaries.add(summary);
        return;
      }

      final TxSummary summary = searchedSummary.get();
      if (!summary.getFromAddress().contains(tx.getFromAddress())) {
        summary.getFromAddress().add(tx.getFromAddress());
      }

      if (!summary.getToAddress().contains(tx.getToAddress())) {
        summary.getToAddress().add(tx.getToAddress());
      }
    });

    return summaries;
  }


  @Override
  @Transactional(readOnly = true)
  public List<TxGraph> getTxsAfterTime() {
    LocalDate localDate = LocalDate.ofInstant(Instant.now(), ZoneOffset.UTC).minusDays(MINUS_DAYS);

    List<TxGraphProjection> txs = txRepository.getTransactionsAfterTime(
        Timestamp.valueOf(localDate.atStartOfDay()));

    if (CollectionUtils.isEmpty(txs)) {
      return Collections.emptyList();
    }

    return txs.stream()
        .collect(
            Collectors.groupingByConcurrent(
                tx ->
                    TimeUtil.formatDate(tx.getTime()),
                Collectors.summingInt(TxGraphProjection::getTransactionNo)
            ))
        .entrySet()
        .stream()
        .map(entry -> TxGraph.builder()
            .date(entry.getKey())
            .txs(entry.getValue())
            .build())
        .sorted(Comparator.comparing(TxGraph::getDate))
        .collect(Collectors.toList());

  }

  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<TxFilterResponse> filterTx(TxFilterRequest request, Pageable pageable) {
    Page<Tx> txPage;
    if (request != null) {
      txPage = txRepository.findAll(txSpecification.getFilter(request), pageable);
    } else {
      txPage = txRepository.findAll(pageable);
    }
    return new BaseFilterResponse<>(txPage, mapDataFromTxListToResponseList(txPage));
  }


  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<TxFilterResponse> getTransactionsByAddress(String address, Pageable pageable) {
    Page<Tx> txPage = addressTxBalanceRepository.findAllByAddress(address, pageable);
    return new BaseFilterResponse<>(txPage, mapDataFromTxListToResponseList(txPage));
  }

  @Override
  public BaseFilterResponse<TxFilterResponse> getTransactionsByToken(String tokenId, Pageable pageable) {
    BaseFilterResponse<TxFilterResponse> response = new BaseFilterResponse<>();
    Optional<MultiAsset> multiAsset = multiAssetRepository.findByFingerprint(tokenId);
    if(multiAsset.isPresent()) {
      List<Long> txIds = addressTokenRepository.findTxsByMultiAsset(multiAsset.get(), pageable);
      List<Tx> txList = txRepository.findByIdIn(txIds);
      Page<Tx> txPage = new PageImpl<>(txList, pageable, multiAsset.get().getTxCount());
      List<TxFilterResponse> txFilterResponses = mapDataFromTxListToResponseList(txPage);
      response = new BaseFilterResponse<>(txPage, txFilterResponses);
    }
    return response;
  }

  /**
   * Mapping from tx entity list to tx response dto
   *
   * @param txPage list tx in page
   * @return list tx response
   */
  private List<TxFilterResponse> mapDataFromTxListToResponseList(Page<Tx> txPage) {
    if(CollectionUtils.isEmpty(txPage.getContent())) {
      return new ArrayList<>();
    }
    Set<Long> blockIdList = txPage.getContent().stream().map(Tx::getBlockId)
        .collect(Collectors.toSet());
    var conditions = Specification.where(BlockSpecification.hasIdIn(blockIdList));
    List<Block> blocks = blockRepository.findAll(conditions);
    Map<Long, Block> blockMap = blocks.stream()
        .collect(Collectors.toMap(Block::getId, Function.identity()));
    //get addresses input
    Set<Long> txIdSet = txPage.getContent().stream().map(Tx::getId).collect(Collectors.toSet());
    List<AddressInputOutputProjection> txInList = txOutRepository.findAddressInputListByTxId(
        txIdSet);
    Map<Long, List<AddressInputOutputProjection>> addressInMap = txInList.stream()
        .collect(Collectors.groupingBy(AddressInputOutputProjection::getTxId));

    //get addresses output
    List<AddressInputOutputProjection> txOutList = txOutRepository.findAddressOutputListByTxId(
        txIdSet);
    Map<Long, List<AddressInputOutputProjection>> addressOutMap = txOutList.stream()
        .collect(Collectors.groupingBy(AddressInputOutputProjection::getTxId));

    List<TxFilterResponse> txFilterResponses = new ArrayList<>();
    for (Tx tx : txPage.getContent()) {
      Long txId = tx.getId();
      if(blockMap.containsKey(tx.getBlockId())) {
        tx.setBlock(blockMap.get(tx.getBlockId()));
      }
      TxFilterResponse txResponse = txMapper.txToTxFilterResponse(tx);
      if (addressOutMap.containsKey(txId)) {
        txResponse.setAddressesOutput(
            addressOutMap.get(tx.getId()).stream().map(AddressInputOutputProjection::getAddress)
                .collect(Collectors.toList()));
      } else {
        txResponse.setAddressesOutput(new ArrayList<>());
      }
      if (addressInMap.containsKey(txId)) {
        txResponse.setAddressesInput(
            addressInMap.get(tx.getId()).stream().map(AddressInputOutputProjection::getAddress)
                .collect(Collectors.toList()));
      } else {
        txResponse.setAddressesInput(new ArrayList<>());
      }
      txFilterResponses.add(txResponse);
    }
    return txFilterResponses;
  }

  @Override
  @Transactional(readOnly = true)
  public TxResponse getTxDetailByHash(String hash) {
    Tx tx = txRepository.findByHash(hash).orElseThrow(
        () -> new BusinessException(BusinessCode.TRANSACTION_NOT_FOUND)
    );
    Integer currentBlockNo = blockRepository.findCurrentBlock().orElseThrow(
        () -> new BusinessException(BusinessCode.BLOCK_NOT_FOUND)
    );
    TxResponse txResponse = txMapper.txToTxResponse(tx);

    if(Objects.nonNull(txResponse.getTx().getEpochNo())) {
      Epoch epoch = epochRepository.findFirstByNo(txResponse.getTx().getEpochNo()).orElseThrow(
          () -> new BusinessException(BusinessCode.EPOCH_NOT_FOUND)
      );
      txResponse.getTx().setMaxEpochSlot(epoch.getMaxSlot());
    }
    if(Objects.nonNull(txResponse.getTx().getBlockNo())) {
      txResponse.getTx().setConfirmation(currentBlockNo - txResponse.getTx().getBlockNo());
    } else {
      txResponse.getTx().setConfirmation(currentBlockNo);
    }
    txResponse.getTx().setStatus(TxStatus.SUCCESS);

    // get address input output
    getSummaryAndUTxOs(tx, txResponse);
    getContracts(tx, txResponse);
    getCollaterals(tx, txResponse);
    getWithdrawals(tx, txResponse);
    getDelegations(tx, txResponse);
    getMints(tx, txResponse);
    return txResponse;
  }

  /**
   * Get transaction mints info
   *
   * @param tx         transaction
   * @param txResponse response data of transaction
   */
  private void getMints(Tx tx, TxResponse txResponse) {
    List<MaTxMint> maTxMints = maTxMintRepository.findByTx(tx);
    Set<String> subjects = maTxMints.stream().map(
        ma -> ma.getIdent().getPolicy() + ma.getIdent().getName()).collect(Collectors.toSet());
    var assetMetadataList = assetMetadataRepository.findBySubjectIn(subjects);
    var assetMetadataMap = assetMetadataList.stream().collect(Collectors.toMap(
        AssetMetadata::getSubject, Function.identity()
    ));
    if (!CollectionUtils.isEmpty(maTxMints)) {
      txResponse.setMints(maTxMints.stream().map(
          ma -> {
            TxMintingResponse txMintingResponse = maTxMintMapper.fromMaTxMint(ma);
            String subject = ma.getIdent().getPolicy() + ma.getIdent().getName();
            txMintingResponse.setMetadata(assetMetadataMapper.fromAssetMetadata(assetMetadataMap.get(subject)));
            return txMintingResponse;
          }).collect(Collectors.toList()));
    }
  }

  /**
   * Get transaction delegations info
   *
   * @param tx         transaction
   * @param txResponse response data of transaction
   */
  private void getDelegations(Tx tx, TxResponse txResponse) {
    List<Delegation> delegations = delegationRepository.findByTx(tx);
    if (!CollectionUtils.isEmpty(delegations)) {
      txResponse.setDelegations(delegations.stream().map(delegationMapper::fromDelegation)
          .collect(Collectors.toList()));
    }
  }

  /**
   * Get transaction withdrawals info
   *
   * @param tx         transaction
   * @param txResponse response data of transaction
   */
  private void getWithdrawals(Tx tx, TxResponse txResponse) {
    List<Withdrawal> withdrawals = withdrawalRepository.findByTx(tx);
    if (!CollectionUtils.isEmpty(withdrawals)) {
      List<String> addressToList = txResponse.getUTxOs().getOutputs().stream()
          .map(TxOutResponse::getAddress).collect(Collectors.toList());
      List<WithdrawalResponse> withdrawalResponses = withdrawals.stream().map(withdrawal -> {
        WithdrawalResponse withdrawalResponse = withdrawalMapper.fromWithdrawal(withdrawal);
        withdrawalResponse.setAddressTo(addressToList);
        return withdrawalResponse;
      }).collect(Collectors.toList());

      txResponse.setWithdrawals(withdrawalResponses);
    }
  }

  /**
   * Get transaction collaterals info
   *
   * @param tx         transaction
   * @param txResponse response data of transaction
   */
  private void getCollaterals(Tx tx, TxResponse txResponse) {
    List<CollateralInputOutputProjection> collateralInputs = collateralTxInRepository
        .findTxCollateralInput(tx);
    if (!CollectionUtils.isEmpty(collateralInputs)) {
      List<CollateralResponse> collateralInputResponses = collateralInputs.stream().map(
          collateralInputOutputProjection -> CollateralResponse.builder()
              .txHash(collateralInputOutputProjection.getTxHash())
              .amount(collateralInputOutputProjection.getValue())
              .address(collateralInputOutputProjection.getAddress())
              .build()
      ).collect(Collectors.toList());
      txResponse.setCollaterals(collateralInputResponses);
    }
  }

  /**
   * Get transaction contracts info
   *
   * @param tx         transaction
   * @param txResponse response data of transaction
   */
  private void getContracts(Tx tx, TxResponse txResponse) {
    List<TxContractProjection> redeemers = redeemerRepository.findContractByTx(tx);
    if (!CollectionUtils.isEmpty(redeemers)) {
      List<ContractResponse> contractResponses = redeemers.stream().map(redeemer -> {
        if (redeemer.getPurpose().equals(ScriptPurposeType.SPEND)) {
          return new ContractResponse(redeemer.getAddress());
        } else {
          return new ContractResponse(redeemer.getScriptHash());
        }
      }).collect(Collectors.toList());
      txResponse.setContracts(contractResponses);
    }
  }

  /**
   * Get transaction summary and UTxOs info
   *
   * @param tx         transaction
   * @param txResponse response data of transaction
   */
  private void getSummaryAndUTxOs(Tx tx, TxResponse txResponse) {
    List<AddressInputOutputProjection> addressInputInfo = txOutRepository.getTxAddressInputInfo(
        tx);
    Map<TxOutResponse, List<AddressInputOutputProjection>> addressInputMap = addressInputInfo.stream()
        .collect(Collectors.groupingBy(
            txOutMapper::fromAddressInputOutput
        ));
    List<AddressInputOutputProjection> addressOutputInfo = txOutRepository.getTxAddressOutputInfo(
        tx);
    Map<TxOutResponse, List<AddressInputOutputProjection>> addressOutputMap = addressOutputInfo.stream()
        .collect(Collectors.groupingBy(
            txOutMapper::fromAddressInputOutput
        ));
    List<TxOutResponse> uTxOOutputs = mappingProjectionToAddress(addressOutputMap);
    List<TxOutResponse> uTxOInputs = mappingProjectionToAddress(addressInputMap);
    UTxOResponse uTxOs = UTxOResponse.builder()
        .inputs(uTxOInputs)
        .outputs(uTxOOutputs)
        .build();
    txResponse.setUTxOs(uTxOs);
    SummaryResponse summary = SummaryResponse.builder()
        .stakeAddressTxInputs(getStakeAddressInfo(addressInputInfo))
        .stakeAddressTxOutputs(getStakeAddressInfo(addressOutputInfo))
        .build();
    txResponse.setSummary(summary);

  }

  /**
   * Map data from AddressInputOutputProjection to TxOutResponse
   *
   * @param addressInputOutputMap address projection map
   * @return address response
   */
  private List<TxOutResponse> mappingProjectionToAddress(
      Map<TxOutResponse, List<AddressInputOutputProjection>> addressInputOutputMap) {
    List<TxOutResponse> uTxOs = new ArrayList<>(addressInputOutputMap.keySet());
    for (TxOutResponse uTxO : uTxOs) {
      List<TxMintingResponse> tokens = addressInputOutputMap.get(uTxO).stream().
          filter(token -> Objects.nonNull(token.getAssetId())).map(
              maTxMintMapper::fromAddressInputOutputProjection
          ).collect(Collectors.toList());
      uTxO.setTokens(tokens);
    }
    return uTxOs;
  }

  /**
   * Get stake address info from address
   *
   * @param addressInfo List address input or output info
   * @return list stake address input or output info
   */
  private List<TxOutResponse> getStakeAddressInfo(
      List<AddressInputOutputProjection> addressInfo) {

    Map<TxOutResponse, List<AddressInputOutputProjection>> addressMap = addressInfo.stream()
        .collect(Collectors.groupingBy(
            txOutMapper::fromStakeAddressInputOutput
        ));
    Map<String, List<AddressInputOutputProjection>> addressTokenMap = addressInfo.stream()
        .collect(Collectors.groupingBy(
            AddressInputOutputProjection::getStakeAddress
        ));
    var addressValueMap = addressMap.keySet().stream().collect(Collectors.groupingBy(
        TxOutResponse::getAddress,
        Collectors.reducing(BigDecimal.ZERO, TxOutResponse::getValue,
            BigDecimal::add)
    ));
    List<TxOutResponse> stakeAddressTxInputList = new ArrayList<>();
    addressValueMap.forEach(
        (key, value) -> stakeAddressTxInputList.add(TxOutResponse.builder()
            .address(key)
            .value(value)
            .tokens(addressTokenMap.get(key).stream()
                .filter(token -> Objects.nonNull(token.getAssetId()))
                .map(maTxMintMapper::fromAddressInputOutputProjection).collect(Collectors.toList()))
            .build())
    );
    return stakeAddressTxInputList;
  }
}
