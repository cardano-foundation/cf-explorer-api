package org.cardanofoundation.explorer.api.service.impl;

import com.bloxbean.cardano.client.util.AssetUtil;
import jakarta.annotation.PostConstruct;
import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.common.enumeration.CertificateType;
import org.cardanofoundation.explorer.api.common.enumeration.TxStatus;
import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.mapper.AssetMetadataMapper;
import org.cardanofoundation.explorer.api.mapper.DelegationMapper;
import org.cardanofoundation.explorer.api.mapper.MaTxMintMapper;
import org.cardanofoundation.explorer.api.mapper.ProtocolMapper;
import org.cardanofoundation.explorer.api.mapper.TokenMapper;
import org.cardanofoundation.explorer.api.mapper.TxMapper;
import org.cardanofoundation.explorer.api.mapper.TxOutMapper;
import org.cardanofoundation.explorer.api.mapper.WithdrawalMapper;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.TxFilterResponse;
import org.cardanofoundation.explorer.api.model.response.dashboard.TxGraph;
import org.cardanofoundation.explorer.api.model.response.dashboard.TxSummary;
import org.cardanofoundation.explorer.api.model.response.pool.PoolRelayResponse;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolDeRegistrationProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolRelayProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolUpdateDetailProjection;
import org.cardanofoundation.explorer.api.model.response.pool.projection.StakeKeyProjection;
import org.cardanofoundation.explorer.api.model.response.token.TokenAddressResponse;
import org.cardanofoundation.explorer.api.model.response.tx.CollateralResponse;
import org.cardanofoundation.explorer.api.model.response.tx.ContractResponse;
import org.cardanofoundation.explorer.api.model.response.tx.SummaryResponse;
import org.cardanofoundation.explorer.api.model.response.tx.TxMintingResponse;
import org.cardanofoundation.explorer.api.model.response.tx.TxOutResponse;
import org.cardanofoundation.explorer.api.model.response.tx.TxPoolCertificate;
import org.cardanofoundation.explorer.api.model.response.tx.TxResponse;
import org.cardanofoundation.explorer.api.model.response.tx.TxStakeCertificate;
import org.cardanofoundation.explorer.api.model.response.tx.UTxOResponse;
import org.cardanofoundation.explorer.api.model.response.tx.WithdrawalResponse;
import org.cardanofoundation.explorer.api.projection.AddressInputOutputProjection;
import org.cardanofoundation.explorer.api.projection.TxContractProjection;
import org.cardanofoundation.explorer.api.projection.TxGraphProjection;
import org.cardanofoundation.explorer.api.projection.TxIOProjection;
import org.cardanofoundation.explorer.api.projection.TxLimit;
import org.cardanofoundation.explorer.api.repository.AddressRepository;
import org.cardanofoundation.explorer.api.repository.AddressTokenRepository;
import org.cardanofoundation.explorer.api.repository.AddressTxBalanceRepository;
import org.cardanofoundation.explorer.api.repository.AssetMetadataRepository;
import org.cardanofoundation.explorer.api.repository.BlockRepository;
import org.cardanofoundation.explorer.api.repository.DelegationRepository;
import org.cardanofoundation.explorer.api.repository.EpochRepository;
import org.cardanofoundation.explorer.api.repository.FailedTxOutRepository;
import org.cardanofoundation.explorer.api.repository.MaTxMintRepository;
import org.cardanofoundation.explorer.api.repository.MultiAssetRepository;
import org.cardanofoundation.explorer.api.repository.ParamProposalRepository;
import org.cardanofoundation.explorer.api.repository.PoolRelayRepository;
import org.cardanofoundation.explorer.api.repository.PoolRetireRepository;
import org.cardanofoundation.explorer.api.repository.PoolUpdateRepository;
import org.cardanofoundation.explorer.api.repository.RedeemerRepository;
import org.cardanofoundation.explorer.api.repository.StakeDeRegistrationRepository;
import org.cardanofoundation.explorer.api.repository.StakeRegistrationRepository;
import org.cardanofoundation.explorer.api.repository.TxOutRepository;
import org.cardanofoundation.explorer.api.repository.TxRepository;
import org.cardanofoundation.explorer.api.repository.UnconsumeTxInRepository;
import org.cardanofoundation.explorer.api.repository.WithdrawalRepository;
import org.cardanofoundation.explorer.api.service.TxService;
import org.cardanofoundation.explorer.api.util.HexUtils;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.cardanofoundation.explorer.consumercommon.entity.Address;
import org.cardanofoundation.explorer.consumercommon.entity.AddressToken;
import org.cardanofoundation.explorer.consumercommon.entity.AddressTxBalance;
import org.cardanofoundation.explorer.consumercommon.entity.AssetMetadata;
import org.cardanofoundation.explorer.consumercommon.entity.BaseEntity_;
import org.cardanofoundation.explorer.consumercommon.entity.Block;
import org.cardanofoundation.explorer.consumercommon.entity.Delegation;
import org.cardanofoundation.explorer.consumercommon.entity.Epoch;
import org.cardanofoundation.explorer.consumercommon.entity.MaTxMint;
import org.cardanofoundation.explorer.consumercommon.entity.MultiAsset;
import org.cardanofoundation.explorer.consumercommon.entity.ParamProposal;
import org.cardanofoundation.explorer.consumercommon.entity.Tx;
import org.cardanofoundation.explorer.consumercommon.entity.Withdrawal;
import org.cardanofoundation.explorer.consumercommon.enumeration.ScriptPurposeType;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import java.math.BigInteger;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.LongStream;
import java.util.stream.Stream;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.util.Pair;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

@Service
@RequiredArgsConstructor
@Log4j2
public class TxServiceImpl implements TxService {

  public static final int ONE_DAY_HOURS = 25;
  private final TxRepository txRepository;
  private final TxOutRepository txOutRepository;
  private final BlockRepository blockRepository;
  private final TxMapper txMapper;
  private final TxOutMapper txOutMapper;
  private final TokenMapper tokenMapper;
  private final RedeemerRepository redeemerRepository;
  private final EpochRepository epochRepository;
  private final UnconsumeTxInRepository unconsumeTxInRepository;
  private final FailedTxOutRepository failedTxOutRepository;
  private final WithdrawalRepository withdrawalRepository;
  private final AddressRepository addressRepository;
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
  private final ParamProposalRepository paramProposalRepository;
  private final StakeRegistrationRepository stakeRegistrationRepository;
  private final StakeDeRegistrationRepository stakeDeRegistrationRepository;
  private final PoolUpdateRepository poolUpdateRepository;
  private final PoolRelayRepository poolRelayRepository;
  private final PoolRetireRepository poolRetireRepository;
  private final ProtocolMapper protocolMapper;

  private final RedisTemplate<String, TxGraph> redisTemplate;
  @Value("${application.network}")
  private String network;
  private static final int SUMMARY_SIZE = 4;
  private static final long MONTH = 32;
  private static final String TRANSACTION_GRAPH_MONTH_KEY = "TRANSACTION_GRAPH_MONTH";
  private static final String TRANSACTION_GRAPH_DAY_KEY = "TRANSACTION_GRAPH_DAY_KEY";
  private final ObjectMapper objectMapper;

  @Override
  @Transactional(readOnly = true)
  public List<TxSummary> findLatestTxSummary() {
    List<Long> txIds = txRepository.findLatestTxId(
        PageRequest.of(BigInteger.ZERO.intValue(),
            SUMMARY_SIZE,
            Sort.by(BaseEntity_.ID).descending()));

    if (txIds.isEmpty()) {
      return Collections.emptyList();
    }

    List<TxSummary> summaries = new ArrayList<>();
    List<TxIOProjection> txs = txRepository.findLatestTxIO(txIds);

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
            .epochNo(tx.getEpochNo())
            .epochSlotNo(tx.getEpochSlotNo())
            .slot(tx.getSlot())
            .time(tx.getTime())
            .status(Boolean.TRUE.equals(tx.getValidContract()) ? TxStatus.SUCCESS : TxStatus.FAIL)
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
  public List<TxGraph> getTxsAfterTime(long minusDays) {
    if (minusDays != BigInteger.ONE.longValue()) {
      return getTxGraphsInDays(minusDays);
    }
    return getTxGraphsToday();
  }

  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<TxFilterResponse> getAll(Pageable pageable) {
    Page<Tx> txPage = txRepository.findAllTx(pageable);
    return new BaseFilterResponse<>(txPage, mapDataFromTxListToResponseList(txPage));
  }


  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<TxFilterResponse> getTransactionsByBlock(String blockId,
      Pageable pageable) {
    Page<Tx> txPage;
    try {
      Long blockNo = Long.parseLong(blockId);
      txPage = txRepository.findByBlockNo(blockNo, pageable);
    } catch (NumberFormatException e) {
      txPage = txRepository.findByBlockHash(blockId, pageable);
    }
    return new BaseFilterResponse<>(txPage, mapDataFromTxListToResponseList(txPage));
  }

  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<TxFilterResponse> getTransactionsByAddress(String address,
      Pageable pageable) {
    Address addr = addressRepository.findFirstByAddress(address).orElseThrow(
        () -> new BusinessException(BusinessCode.ADDRESS_NOT_FOUND)
    );
    List<Tx> txList = addressTxBalanceRepository.findAllByAddress(addr, pageable);

    Page<Tx> txPage = new PageImpl<>(txList, pageable, addr.getTxCount());
    return new BaseFilterResponse<>(txPage, mapDataFromTxListToResponseList(txPage, address));
  }

  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<TxFilterResponse> getTransactionsByToken(String tokenId,
      Pageable pageable) {
    BaseFilterResponse<TxFilterResponse> response = new BaseFilterResponse<>();
    Optional<MultiAsset> multiAsset = multiAssetRepository.findByFingerprint(tokenId);
    if (multiAsset.isPresent()) {
      List<Long> txIds = addressTokenRepository.findTxsByMultiAsset(multiAsset.get(), pageable);
      List<Tx> txList = txRepository.findByIdIn(txIds);
      Page<Tx> txPage = new PageImpl<>(txList, pageable, multiAsset.get().getTxCount());
      List<TxFilterResponse> txFilterResponses = mapDataFromTxListToResponseList(txPage);
      response = new BaseFilterResponse<>(txPage, txFilterResponses);
    }
    return response;
  }

  @Override
  public BaseFilterResponse<TxFilterResponse> getTransactionsByStake(String stakeKey,
      Pageable pageable) {
    Page<Tx> txPage = addressTxBalanceRepository.findAllByStake(stakeKey, pageable);
    return new BaseFilterResponse<>(txPage, mapDataFromTxListToResponseList(txPage));
  }

  /**
   * Mapping from tx entity list to tx response dto
   *
   * @param txPage list tx in page
   * @return list tx response
   */
  private List<TxFilterResponse> mapDataFromTxListToResponseList(Page<Tx> txPage) {
    if (CollectionUtils.isEmpty(txPage.getContent())) {
      return new ArrayList<>();
    }
    Set<Long> blockIdList = txPage.getContent().stream().map(Tx::getBlockId)
        .collect(Collectors.toSet());
    List<Block> blocks = blockRepository.findAllByIdIn(blockIdList);
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
      if (blockMap.containsKey(tx.getBlockId())) {
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

  private List<TxFilterResponse> mapDataFromTxListToResponseList(Page<Tx> txPage, String address) {
    List<TxFilterResponse> txFilterResponses = mapDataFromTxListToResponseList(txPage);

    Set<Long> txIdList = txPage.getContent().stream().map(Tx::getId).collect(Collectors.toSet());

    // get address tx balance
    List<AddressTxBalance> addressTxBalances =
        addressTxBalanceRepository.findByTxIdInAndByAddress(txIdList, address);
    Map<Long, AddressTxBalance> addressTxBalanceMap =
        addressTxBalances.stream()
            .collect(Collectors.toMap(AddressTxBalance::getTxId, Function.identity()));

    // get address token
    List<AddressToken> addressTokens =
        addressTokenRepository.findByTxIdInAndByAddress(txIdList, address);
    Map<Long, List<AddressToken>> addressTokenMap =
        addressTokens.stream()
            .collect(Collectors.groupingBy(addressToken -> addressToken.getTx().getId()));

    // get metadata
    List<MultiAsset> multiAssets = addressTokens.stream().map(AddressToken::getMultiAsset).toList();
    Set<String> subjects = multiAssets.stream().map(
        ma -> ma.getPolicy() + ma.getName()).collect(Collectors.toSet());
    List<AssetMetadata> assetMetadataList = assetMetadataRepository.findBySubjectIn(subjects);
    Map<String, AssetMetadata> assetMetadataMap = assetMetadataList.stream().collect(
        Collectors.toMap(AssetMetadata::getSubject, Function.identity()));

    txFilterResponses.forEach(
        tx -> {
          if (addressTxBalanceMap.containsKey(tx.getId())) {
            tx.setBalance(addressTxBalanceMap.get(tx.getId()).getBalance());
          }
          if (addressTokenMap.containsKey(tx.getId())) {
            List<TokenAddressResponse> tokenAddressResponseList =
                addressTokenMap.get(tx.getId()).stream()
                    .map(
                        addressToken -> {
                          TokenAddressResponse tokenAddressResponse =
                              tokenMapper.fromMultiAssetAndAddressToken(
                                  addressToken.getMultiAsset(), addressToken);
                          String subject =
                              addressToken.getMultiAsset().getPolicy()
                                  + addressToken.getMultiAsset().getName();
                          tokenAddressResponse.setMetadata(
                              assetMetadataMapper.fromAssetMetadata(assetMetadataMap.get(subject)));
                          return tokenAddressResponse;
                        })
                    .toList();
            tx.setTokens(tokenAddressResponseList);
          }
        });

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

    if (Objects.nonNull(txResponse.getTx().getEpochNo())) {
      Epoch epoch = epochRepository.findFirstByNo(txResponse.getTx().getEpochNo()).orElseThrow(
          () -> new BusinessException(BusinessCode.EPOCH_NOT_FOUND)
      );
      txResponse.getTx().setMaxEpochSlot(epoch.getMaxSlot());
    }
    if (Objects.nonNull(txResponse.getTx().getBlockNo())) {
      txResponse.getTx().setConfirmation(currentBlockNo - txResponse.getTx().getBlockNo());
    } else {
      txResponse.getTx().setConfirmation(currentBlockNo);
    }

    // get address input output
    getSummaryAndUTxOs(tx, txResponse);
    getContracts(tx, txResponse);
    getCollaterals(tx, txResponse);
    getWithdrawals(tx, txResponse);
    getDelegations(tx, txResponse);
    getMints(tx, txResponse);
    getProtocols(tx, txResponse);
    getStakeCertificates(tx, txResponse);
    getPoolCertificates(tx, txResponse);
    /*
     * If the transaction is invalid, the collateral is the input and the output of the transaction.
     * Otherwise, the collateral is the input and the output of the collateral.
     */
    if (Boolean.TRUE.equals(tx.getValidContract())) {
      txResponse.getTx().setStatus(TxStatus.SUCCESS);
    } else {
      txResponse.getTx().setStatus(TxStatus.FAIL);
      CollateralResponse collateralResponse = txResponse.getCollaterals();
      List<TxOutResponse> collateralInputs = collateralResponse.getCollateralInputResponses();
      List<TxOutResponse> collateralOutputs = collateralResponse.getCollateralOutputResponses();
      collateralResponse.setCollateralInputResponses(txResponse.getUTxOs().getInputs());
      collateralResponse.setCollateralOutputResponses(txResponse.getUTxOs().getOutputs());
      txResponse.setCollaterals(collateralResponse);
      UTxOResponse uTxOResponse = new UTxOResponse();
      uTxOResponse.setInputs(collateralInputs);
      uTxOResponse.setOutputs(collateralOutputs);
      txResponse.setUTxOs(uTxOResponse);
    }

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
            txMintingResponse.setMetadata(
                assetMetadataMapper.fromAssetMetadata(assetMetadataMap.get(subject)));
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
    CollateralResponse collateralResponse = new CollateralResponse();
    List<AddressInputOutputProjection> collateralInputs = unconsumeTxInRepository
        .findTxCollateralInput(tx);
    List<AddressInputOutputProjection> collateralOutputs = failedTxOutRepository
        .findFailedTxOutByTx(tx);
    if (!CollectionUtils.isEmpty(collateralInputs)) {
      Map<TxOutResponse, List<AddressInputOutputProjection>> addressInputMap = collateralInputs
          .stream().collect(Collectors.groupingBy(
              txOutMapper::fromAddressInputOutput
          ));
      List<TxOutResponse> collateralInputResponse = mappingProjectionToAddress(addressInputMap);
      collateralResponse.setCollateralInputResponses(collateralInputResponse);
    }
    if (!CollectionUtils.isEmpty(collateralOutputs)) {
      Map<TxOutResponse, List<AddressInputOutputProjection>> addressOutputMap = collateralOutputs
          .stream().collect(Collectors.groupingBy(
              txOutMapper::fromAddressInputOutput
          ));
      List<TxOutResponse> collateralOutputResponses = mappingProjectionToAddress(addressOutputMap);
      collateralResponse.setCollateralOutputResponses(collateralOutputResponses);
    }
    if (CollectionUtils.isEmpty(collateralInputs) && CollectionUtils.isEmpty(collateralOutputs)) {
      txResponse.setCollaterals(null);
    } else {
      txResponse.setCollaterals(collateralResponse);
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

    List<TxOutResponse> addressesInfoInput = getStakeAddressInfo(addressInputInfo);
    List<TxOutResponse> addressesInfoOutput = getStakeAddressInfo(addressOutputInfo);

    List<TxOutResponse> stakeAddress = removeDuplicateTx(addressesInfoInput, addressesInfoOutput);

    SummaryResponse summary =
        SummaryResponse.builder()
            .stakeAddress(stakeAddress)
            .build();

    txResponse.setSummary(summary);

  }

  private List<TxOutResponse> removeDuplicateTx(
      List<TxOutResponse> addressesInputs, List<TxOutResponse> addressesOutputs) {
    addressesInputs.forEach(
        txOutResponse -> {
          txOutResponse.setValue(txOutResponse.getValue().multiply(BigInteger.valueOf(-1)));
          txOutResponse
              .getTokens()
              .forEach(
                  token ->
                      token.setAssetQuantity(
                          token.getAssetQuantity().multiply(BigInteger.valueOf(-1))));
        });

    Map<String, List<TxOutResponse>> unionTxsByAddress =
        Stream.concat(addressesInputs.stream(), addressesOutputs.stream())
            .collect(
                Collectors.groupingBy(
                    TxOutResponse::getAddress, Collectors.toCollection(ArrayList::new)));

    List<TxOutResponse> summary = new ArrayList<>();

    unionTxsByAddress.forEach(
        (address, txs) -> {
          if (txs.size() > 1) {
            BigInteger totalValue = txs.get(0).getValue().add(txs.get(1).getValue());
            txs.get(0).setValue(totalValue);

            Map<String, List<TxMintingResponse>> unionTokenByAsset =
                Stream.concat(txs.get(0).getTokens().stream(), txs.get(1).getTokens().stream())
                    .collect(
                        Collectors.groupingBy(
                            TxMintingResponse::getAssetId,
                            Collectors.toCollection(ArrayList::new)));

            List<TxMintingResponse> tokenResponse = new ArrayList<>();

            unionTokenByAsset.forEach(
                (asset, tokens) -> {
                  if (tokens.size() > 1) {
                    BigInteger totalQuantity =
                        tokens.get(0).getAssetQuantity().add(tokens.get(1).getAssetQuantity());
                    tokens.get(0).setAssetQuantity(totalQuantity);
                  }

                  tokenResponse.add(tokens.get(0));
                });

            txs.get(0).setTokens(tokenResponse);
          }

          summary.add(txs.get(0));
        });

    return summary;
  }

  /**
   * Get protocol params update in transaction
   *
   * @param tx transaction
   * @param txResponse response data of transaction
   */
  private void getProtocols(Tx tx, TxResponse txResponse) {
    List<ParamProposal> paramProposals = paramProposalRepository
        .getParamProposalByRegisteredTxId(tx.getId());

    if (!ObjectUtils.isEmpty(paramProposals)) {
      List<ParamProposal> previousParam =
          paramProposalRepository.getParamProposalBySmallerThanRegisteredTxId(tx.getId());

      txResponse.setProtocols(protocolMapper.mapProtocolParamResponse(previousParam));
      //get previous value
      if(Objects.nonNull(txResponse.getProtocols())){
        paramProposals.removeAll(previousParam);

        txResponse.setPreviousProtocols(
            protocolMapper.mapPreviousProtocolParamResponse(previousParam
                , txResponse.getProtocols()));
      }
    }
  }

  /**
   * Get transaction stake certificates info
   *
   * @param tx         transaction
   * @param txResponse response data of stake certificates
   */
  private void getStakeCertificates(Tx tx, TxResponse txResponse) {
    List<TxStakeCertificate> stakeCertificates = stakeRegistrationRepository
        .findByTx(tx).stream().map(
            item -> new TxStakeCertificate(item.getAddr().getView(),
                CertificateType.STAKE_REGISTRATION)
        ).collect(Collectors.toList());
    stakeCertificates.addAll(stakeDeRegistrationRepository.findByTx(tx).stream()
        .map(
            item -> new TxStakeCertificate(item.getAddr().getView(),
                CertificateType.STAKE_DEREGISTRATION)
        ).collect(Collectors.toList()));
    if(!CollectionUtils.isEmpty(stakeCertificates)){
      txResponse.setStakeCertificates(stakeCertificates);
    }
  }

  /**
   * Get transaction pool certificates info
   * @param tx transaction
   * @param txResponse response data of pool certificates
   */
  private void getPoolCertificates(Tx tx, TxResponse txResponse) {

    // get pool registration
    List<PoolUpdateDetailProjection> poolUpdateDetailProjections = poolUpdateRepository
        .findByTx(tx);
    Set<Long> poolUpdateIdSet = poolUpdateDetailProjections.stream().map(
        PoolUpdateDetailProjection::getPoolUpdateId).collect(Collectors.toSet());
    List<StakeKeyProjection> poolOwners = poolUpdateRepository.findOwnerAccountByPoolUpdate(
        poolUpdateIdSet);
    Map<Long, List<String>> poolOwnerMap = poolOwners.stream().collect(Collectors.groupingBy(
        StakeKeyProjection::getPoolUpdateId,
        Collectors.mapping(StakeKeyProjection::getView, Collectors.toList())
    ));
    List<PoolRelayProjection> poolRelays = poolRelayRepository.findByPoolHashIdIn(poolUpdateIdSet);
    Map<Long, List<PoolRelayProjection>> poolRelayMap =
        poolRelays.stream().collect(Collectors.groupingBy(PoolRelayProjection::getPoolUpdateId));
    List<TxPoolCertificate> poolCertificates = poolUpdateDetailProjections.stream()
        .map(
            item -> {
              List<PoolRelayResponse> relays = null;
              if(poolRelayMap.containsKey(item.getPoolUpdateId())) {
                relays = poolRelayMap.get(item.getPoolUpdateId())
                    .stream().map(relay ->
                        PoolRelayResponse.builder()
                            .dnsName(relay.getDnsName())
                            .dnsSrvName(relay.getDnsSrvName())
                            .ipv4(relay.getIpv4())
                            .ipv6(relay.getIpv6())
                            .port(relay.getPort())
                            .build()).collect(Collectors.toList());
              }
              List<String> poolOwnersList = new ArrayList<>();
              if(poolOwnerMap.containsKey(item.getPoolUpdateId())) {
                poolOwnersList = poolOwnerMap.get(item.getPoolUpdateId());
              }
              return TxPoolCertificate.builder()
                  .poolId(item.getPoolView())
                  .margin(item.getMargin())
                  .rewardAccount(item.getRewardAccount())
                  .pledge(item.getPledge())
                  .vrfKey(item.getVrfKey())
                  .cost(item.getCost())
                  .metadataHash(item.getMetadataHash())
                  .metadataUrl(item.getMetadataUrl())
                  .poolOwners(poolOwnersList)
                  .relays(relays)
                  .type(CertificateType.POOL_REGISTRATION)
                  .build();
            }
        ).collect(Collectors.toList());

    //get pool de registration
    List<PoolDeRegistrationProjection> poolRetireDetailProjections
        = poolRetireRepository.findByAnnouncedTx(tx);
    poolCertificates.addAll(poolRetireDetailProjections.stream().map(
        item -> TxPoolCertificate.builder()
            .poolId(item.getPoolId())
            .epoch(item.getRetiringEpoch())
            .type(CertificateType.POOL_DEREGISTRATION)
            .build()
    ).collect(Collectors.toList()));
    if(!CollectionUtils.isEmpty(poolCertificates)){
      txResponse.setPoolCertificates(poolCertificates);
    }
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
      tokens.addAll(getTokenInFailedTxOut(addressInputOutputMap, uTxO));
      uTxO.setTokens(tokens);
    }
    return uTxOs;
  }

  /**
   * If data from  collateral output, handle token in json string data Otherwise, not exist json
   * string data Example of json string data:
   * [{"unit":"LOVELACE","policyId":"","assetName":"TE9WRUxBQ0U=","quantity":2726335}]
   *
   * @param addressInputOutputMap address projection map
   * @param txOut                 list collateral output
   */
  private List<TxMintingResponse> getTokenInFailedTxOut(
      Map<TxOutResponse, List<AddressInputOutputProjection>> addressInputOutputMap,
      TxOutResponse txOut) {
    List<TxMintingResponse> tokens = new ArrayList<>();
    List<String> tokenStringList = addressInputOutputMap.get(txOut).stream().map(
        AddressInputOutputProjection::getAssetsJson).filter(
        StringUtils::isNotEmpty
    ).collect(Collectors.toList());

    tokenStringList.forEach(tokenString -> {
          if (StringUtils.isNotEmpty(tokenString)) {
            try {
              JsonNode tokenFailedTxOut = objectMapper.readValue(tokenString, JsonNode.class);
              for (JsonNode token : tokenFailedTxOut) {
                String[] policyAndName = token.get("unit").asText().split("\\.");
                if (CommonConstant.LOVELACE.equals(token.get("unit").asText())
                    || policyAndName.length < 2) {
                  continue;
                }
                String assetName = policyAndName[1];
                String assetDisplayName = HexUtils.fromHex(assetName,
                    AssetUtil.calculateFingerPrint(token.get("policyId").asText(), assetName));
                TxMintingResponse txMintingResponse = TxMintingResponse.builder()
                    .assetId(token.get("unit").asText())
                    .assetName(assetDisplayName)
                    .policy(token.get("policyId").asText())
                    .assetQuantity(new BigInteger(token.get("quantity").asText()))
                    .build();
                tokens.add(txMintingResponse);
              }
            } catch (JsonProcessingException e) {
              log.error("Failed to parse token json string: {}", tokenString);
            }
          }
        }
    );
    return tokens;
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
        Collectors.reducing(BigInteger.ZERO, TxOutResponse::getValue,
            BigInteger::add)
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

  private String getRedisKey(String rawKey) {
    return String.join("_", this.network, rawKey);
  }

  private List<TxGraph> getTxGraphsToday() {

    var streamTxGraph = IntStream.range(BigInteger.ONE.intValue(), ONE_DAY_HOURS - 1)
        .boxed()
        .map(hour -> {
          LocalDateTime markTime = LocalDateTime.now(ZoneOffset.UTC).minus(hour, ChronoUnit.HOURS);
          LocalDateTime endTime = LocalDateTime.now(ZoneOffset.UTC)
              .minus(hour + 1L, ChronoUnit.HOURS);

          markTime = LocalDateTime.of(markTime.toLocalDate(),
              LocalTime.of(markTime.getHour(), 0, 0));
          endTime = LocalDateTime.of(endTime.toLocalDate(), LocalTime.of(endTime.getHour(), 0, 0));

          return getTxGraph(Pair.of(markTime, endTime), Boolean.TRUE);
        });

    LocalDateTime currentHour = LocalDateTime.now(ZoneOffset.UTC);

    LocalDateTime endTime = LocalDateTime.now(ZoneOffset.UTC);
    endTime = LocalDateTime.of(endTime.toLocalDate(), LocalTime.of(endTime.getHour(), 0, 0));

    return Stream.concat(Stream.of(getTxGraph(Pair.of(currentHour, endTime), Boolean.TRUE)),
            streamTxGraph)
        .sorted(Comparator.comparing(TxGraph::getDate))
        .collect(Collectors.toList());
  }

  private List<TxGraph> getTxGraphsInDays(long minusDays) {

    var txGraphs = LongStream.range(BigInteger.ZERO.longValue(), minusDays)
        .boxed()
        .parallel()
        .map(day -> {
          LocalDateTime markTime = LocalDateTime.now(ZoneOffset.UTC).minusDays(day)
              .toLocalDate().atStartOfDay();
          LocalDateTime endTime = LocalDateTime.now(ZoneOffset.UTC)
              .minusDays(day + BigInteger.ONE.longValue())
              .toLocalDate().atStartOfDay();
          var keyMonth = getRedisKey(TRANSACTION_GRAPH_MONTH_KEY);
          var index = endTime.getDayOfMonth() % MONTH;
          var redisGraph = redisTemplate.opsForList().index(keyMonth, index);

          if (Objects.nonNull(redisGraph)) {
            var distance = Math.abs(
                ChronoUnit.DAYS.between(
                    LocalDateTime.ofInstant(redisGraph.getDate().toInstant(),
                        ZoneId.systemDefault()),
                    endTime));
            if (distance == BigInteger.ZERO.longValue()) {
              return redisGraph;
            }
          }

          TxGraph txGraph = getTxGraph(Pair.of(markTime, endTime), Boolean.FALSE);
          redisTemplate.opsForList().set(keyMonth, index - 1, txGraph);
          return txGraph;
        });

    return Stream.concat(txGraphs, Stream.of(getTxGraph(
            Pair.of(LocalDateTime.now(ZoneOffset.UTC),
                LocalDateTime.now(ZoneOffset.UTC).toLocalDate().atStartOfDay()),
            Boolean.FALSE)))
        .sorted(Comparator.comparing(TxGraph::getDate))
        .collect(Collectors.toList());
  }

  private TxGraph getTxGraph(Pair<LocalDateTime, LocalDateTime> markAndEnd, boolean isOneDay) {
    var markTime = markAndEnd.getFirst();
    var endTime = markAndEnd.getSecond();

    Supplier<TxGraphProjection> supplier;
    if (isOneDay) {
      supplier = () -> txRepository.getTransactionsAfterDateTime(Timestamp.valueOf(markTime),
          Timestamp.valueOf(endTime));
    } else {
      supplier = () -> txRepository.getTransactionsAfterDate(
          Timestamp.valueOf(markTime),
          Timestamp.valueOf(endTime));
    }

    TxGraphProjection txGraphProjection = supplier.get();

    if (Objects.isNull(txGraphProjection)) {
      return TxGraph.builder()
          .date(Timestamp.valueOf(endTime))
          .txs(BigInteger.ZERO.intValue())
          .complexTxs(BigInteger.ZERO.intValue())
          .simpleTxs(BigInteger.ZERO.intValue())
          .build();
    }

    TxLimit txLimit = txRepository.findRangeTxIdsByBlockIds(
        List.of(txGraphProjection.getMinBlockId(),
            txGraphProjection.getMaxBlockId()));

    var totalComplexTx = txOutRepository.getTotalTokenAndSmartContract(
        txLimit.getMinId(),
        txLimit.getMaxId());

    return TxGraph.builder()
        .date(Timestamp.valueOf(endTime))
        .txs(txGraphProjection.getTransactionNo())
        .complexTxs(totalComplexTx)
        .simpleTxs(txGraphProjection.getTransactionNo() - totalComplexTx)
        .build();
  }


  @PostConstruct
  public void setup() {
    var keyMonth = getRedisKey(TRANSACTION_GRAPH_MONTH_KEY);
    var size = redisTemplate.opsForList().size(keyMonth);
    if (Objects.isNull(size)) {
      size = 0L;
    }
    if (size < MONTH - 1) {
      LongStream.range(0, MONTH - 1 - size).forEach(time ->
          redisTemplate.opsForList().leftPush(keyMonth, TxGraph.builder()
              .date(new Date())
              .txs(BigInteger.ZERO.intValue())
              .complexTxs(BigInteger.ZERO.intValue())
              .simpleTxs(BigInteger.ZERO.intValue())
              .build()));
    }
  }

}
