package com.cardano.explorer.service.impl;

import com.bloxbean.cardano.client.util.AssetUtil;
import com.cardano.explorer.common.constant.CommonConstant;
import com.cardano.explorer.common.enumeration.TxStatus;
import com.cardano.explorer.exception.BusinessCode;
import com.cardano.explorer.mapper.AssetMetadataMapper;
import com.cardano.explorer.mapper.DelegationMapper;
import com.cardano.explorer.mapper.MaTxMintMapper;
import com.cardano.explorer.mapper.TxMapper;
import com.cardano.explorer.mapper.TxOutMapper;
import com.cardano.explorer.mapper.WithdrawalMapper;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.TxFilterResponse;
import com.cardano.explorer.model.response.dashboard.Widget;
import com.cardano.explorer.model.response.tx.CollateralResponse;
import com.cardano.explorer.model.response.tx.TxResponse;
import com.cardano.explorer.model.response.dashboard.TxGraph;
import com.cardano.explorer.model.response.dashboard.TxSummary;
import com.cardano.explorer.model.response.tx.ContractResponse;
import com.cardano.explorer.model.response.tx.SummaryResponse;
import com.cardano.explorer.model.response.tx.TxMintingResponse;
import com.cardano.explorer.model.response.tx.TxOutResponse;
import com.cardano.explorer.model.response.tx.UTxOResponse;
import com.cardano.explorer.model.response.tx.WithdrawalResponse;
import com.cardano.explorer.projection.AddressInputOutputProjection;
import com.cardano.explorer.projection.TxContractProjection;
import com.cardano.explorer.projection.TxGraphProjection;
import com.cardano.explorer.projection.TxIOProjection;
import com.cardano.explorer.repository.AddressRepository;
import com.cardano.explorer.repository.AddressTokenRepository;
import com.cardano.explorer.repository.AddressTxBalanceRepository;
import com.cardano.explorer.repository.AssetMetadataRepository;
import com.cardano.explorer.repository.BlockRepository;
import com.cardano.explorer.repository.UnconsumeTxInRepository;
import com.cardano.explorer.repository.DelegationRepository;
import com.cardano.explorer.repository.EpochRepository;
import com.cardano.explorer.repository.FailedTxOutRepository;
import com.cardano.explorer.repository.MaTxMintRepository;
import com.cardano.explorer.repository.MultiAssetRepository;
import com.cardano.explorer.repository.RedeemerRepository;
import com.cardano.explorer.repository.TxOutRepository;
import com.cardano.explorer.repository.TxRepository;
import com.cardano.explorer.repository.WithdrawalRepository;
import com.cardano.explorer.service.TxService;
import com.cardano.explorer.util.HexUtils;
import com.cardano.explorer.util.TimeUtil;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.Lists;
import com.sotatek.cardano.common.entity.Address;
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
import java.math.BigInteger;
import java.sql.Timestamp;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashSet;
import java.util.IntSummaryStatistics;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;
import java.util.stream.Collectors;
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
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

@Service
@RequiredArgsConstructor
@Log4j2
public class TxServiceImpl implements TxService {

  private final TxRepository txRepository;
  private final TxOutRepository txOutRepository;
  private final BlockRepository blockRepository;
  private final TxMapper txMapper;
  private final TxOutMapper txOutMapper;
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

  private final RedisTemplate<String, TxGraph> redisTemplate;
  @Value("${application.network}")
  private String network;
  private static final int SUMMARY_SIZE = 4;
  private static final long MONTH = 31;

  public static final int BATCH_SIZE = 1000;
  private static final String TRANSACTION_GRAPH_KEY = "TRANSACTION_GRAPH";


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
    return getTxGraphsInTime(minusDays);
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
    return new BaseFilterResponse<>(txPage, mapDataFromTxListToResponseList(txPage));
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
    ObjectMapper objectMapper = new ObjectMapper();
    tokenStringList.forEach(tokenString -> {
          if (StringUtils.isNotEmpty(tokenString)) {
            try {
              JsonNode tokenFailedTxOut = objectMapper.readValue(tokenString, JsonNode.class);
              for (JsonNode token : tokenFailedTxOut) {
                if (CommonConstant.LOVELACE.equals(token.get("unit").asText())) {
                  continue;
                }
                String assetName = token.get("unit").asText().split("\\.")[1];
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

  private List<TxGraph> getTxGraphsInTime(long minusDays) {

    LocalDate markTime = LocalDate.ofInstant(Instant.now(), ZoneOffset.UTC)
        .minusDays(minusDays - 1);

    List<TxGraphProjection> txs =
        txRepository.getTransactionsAfterTime(Timestamp.valueOf(markTime.atStartOfDay()));

    List<Long> blockIdsHaveTransaction =
        txs.stream()
            .map(TxGraphProjection::getBlockId)
            .distinct()
            .collect(Collectors.toList());
    // Get all transaction linked with block ids
    List<Tx> transactions = Collections.synchronizedList(new ArrayList<>());
    Lists.partition(blockIdsHaveTransaction, BATCH_SIZE)
        .parallelStream()
        .forEach(batch -> transactions.addAll(txRepository.findTxIdsByBlockIds(batch)));

    List<Long> transactionIds = transactions.stream().map(Tx::getId).collect(Collectors.toList());
    // calculate transaction Ids Have Token
    Set<Long> transactionIdsHaveToken = Collections.synchronizedSet(new HashSet<>());
    Lists.partition(transactionIds, BATCH_SIZE)
        .parallelStream()
        .forEach(batch ->
            transactionIdsHaveToken.addAll(addressTokenRepository.findTransactionHaveToken(batch)));

    // calculate remain transactionIds have smart contract
    Set<Long> transactionIdHaveSmartContract = Collections.synchronizedSet(new HashSet<>());
    List<Long> transactionRemain = transactionIds.stream()
        .filter(id -> !transactionIdsHaveToken.contains(id))
        .collect(Collectors.toList());
    Lists.partition(transactionRemain, BATCH_SIZE)
        .parallelStream()
        .forEach(batch ->
            transactionIdHaveSmartContract.addAll(
                addressTxBalanceRepository.findTransactionIdsHaveSmartContract(batch,
                    Boolean.TRUE)));

    ConcurrentHashMap<Date, TxGraph> txGraphs = new ConcurrentHashMap<>();

    if (minusDays != BigInteger.ONE.longValue()) {
      Lists.partition(txs, BATCH_SIZE)
          .parallelStream()
          .forEach(batch -> batch.parallelStream().forEach(tx -> {
            var key = TimeUtil.formatDate(tx.getTime());
            TxGraph txGraph;
            if (txGraphs.containsKey(key)) {
              txGraph = txGraphs.get(key);
              txGraph.setTxs(txGraph.getTxs() + tx.getTransactionNo());
            } else {
              log.debug("for date {}", key);
              txGraph = TxGraph.builder()
                  .txs(tx.getTransactionNo())
                  .date(key)
                  .build();
              txGraphs.put(key, txGraph);
            }

            var complexTransaction = getCountComplexTransactions(transactions, transactionIdsHaveToken,
                transactionIdHaveSmartContract, tx);

            txGraph.setComplexTxs((int) complexTransaction);
            txGraph.setSimpleTxs(txGraph.getTxs() - txGraph.getComplexTxs());
          }));
    } else {
      txs.parallelStream()
          .forEach(tx -> {
            var currentDateTime = tx.getTime().toLocalDateTime();
            var key = Timestamp.valueOf(
                LocalDateTime.of(LocalDate.ofYearDay(currentDateTime.getYear(),
                    currentDateTime.getDayOfYear()), LocalTime.of(currentDateTime.getHour(), 0)));

            TxGraph txGraph;
            if (txGraphs.containsKey(key)) {
              txGraph = txGraphs.get(key);
              txGraph.setTxs(txGraph.getTxs() + tx.getTransactionNo());
            } else {
              txGraph = TxGraph.builder()
                  .txs(tx.getTransactionNo())
                  .date(key)
                  .build();
            }

            var complexTransaction = getCountComplexTransactions(transactions,
                transactionIdsHaveToken, transactionIdHaveSmartContract,
                tx);

            txGraph.setComplexTxs((int) complexTransaction);
            txGraph.setSimpleTxs(txGraph.getTxs() - txGraph.getComplexTxs());

            txGraphs.put(key, txGraph);
          });
    }

    return txGraphs.values()
        .stream()
        .sorted(Comparator.comparing(TxGraph::getDate))
        .collect(Collectors.toList());
  }

  private static long getCountComplexTransactions(List<Tx> transactions, Set<Long> transactionIdsHaveToken,
      Set<Long> transactionIdHaveSmartContract, TxGraphProjection tx) {
    return transactions.stream()
        .filter(transaction -> tx.getBlockId().equals(transaction.getBlockId()))
        .filter(transaction -> transactionIdsHaveToken.contains(transaction.getId()) ||
            transactionIdHaveSmartContract.contains(transaction.getId())).count();
  }

}
