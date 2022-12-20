package com.cardano.explorer.service.impl;

import com.cardano.explorer.exception.BusinessCode;
import com.cardano.explorer.mapper.AssetMetadataMapper;
import com.cardano.explorer.mapper.MaTxMintMapper;
import com.cardano.explorer.mapper.TokenMapper;
import com.cardano.explorer.mapper.TxMapper;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.TxFilterResponse;
import com.cardano.explorer.model.response.token.TokenAddressResponse;
import com.cardano.explorer.model.response.token.TokenFilterResponse;
import com.cardano.explorer.model.response.token.TokenMetadataResponse;
import com.cardano.explorer.model.response.token.TokenMintTxResponse;
import com.cardano.explorer.model.response.token.TokenResponse;
import com.cardano.explorer.projection.AddressInputOutputProjection;
import com.cardano.explorer.projection.AddressTokenProjection;
import com.cardano.explorer.repository.AddressTokenRepository;
import com.cardano.explorer.repository.AssetMetadataRepository;
import com.cardano.explorer.repository.BlockRepository;
import com.cardano.explorer.repository.MaTxMintRepository;
import com.cardano.explorer.repository.MultiAssetRepository;
import com.cardano.explorer.repository.TxOutRepository;
import com.cardano.explorer.repository.TxRepository;
import com.cardano.explorer.service.TokenService;
import com.cardano.explorer.specification.BlockSpecification;
import com.sotatek.cardano.common.entity.AssetMetadata;
import com.sotatek.cardano.common.entity.Block;
import com.sotatek.cardano.common.entity.MaTxMint;
import com.sotatek.cardano.common.entity.MultiAsset;
import com.sotatek.cardano.common.entity.Tx;
import com.sotatek.cardanocommonapi.exceptions.BusinessException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
@Log4j2
public class TokenServiceImpl implements TokenService {

  private final TxRepository txRepository;

  private final MultiAssetRepository multiAssetRepository;
  private final MaTxMintRepository maTxMintRepository;
  private final TokenMapper tokenMapper;
  private final MaTxMintMapper maTxMintMapper;

  private final AssetMetadataRepository assetMetadataRepository;
  private final AssetMetadataMapper assetMetadataMapper;
  private final AddressTokenRepository addressTokenRepository;
  private final TxMapper txMapper;
  private final BlockRepository blockRepository;
  private final TxOutRepository txOutRepository;


  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<TokenFilterResponse> filterToken(Pageable pageable) {
    BaseFilterResponse<TokenFilterResponse> response = new BaseFilterResponse<>();
    Page<MultiAsset> multiAssets = multiAssetRepository.findAll(pageable);
    Set<String> subjects = multiAssets.getContent().stream()
        .map(item -> item.getPolicy() + item.getName()).collect(Collectors.toSet());
    List<AssetMetadata> assetMetadataList
        = assetMetadataRepository.findBySubjectIn(subjects);
    Map<String, TokenMetadataResponse> tokenMetadataResponseMap
        = assetMetadataList.stream().collect(Collectors.toMap(AssetMetadata::getSubject,
        assetMetadataMapper::fromAssetMetadata));
    List<TokenFilterResponse> tokenFilterResponses = multiAssets.stream().map( item -> {
        TokenFilterResponse tokenFilterResponse = tokenMapper.fromMultiAssetToFilterResponse(item);
        tokenFilterResponse.setMetadata(tokenMetadataResponseMap.get(item.getPolicy() + item.getName()));
        return tokenFilterResponse;
    }).collect(Collectors.toList());
    response.setData(tokenFilterResponses);


    response.setCurrentPage(pageable.getPageNumber());
    response.setTotalItems(multiAssets.getTotalElements());
    response.setTotalPages(multiAssets.getTotalPages());
    return response;
  }

  /**
   * Get token detail info by token id
   *
   * @param tokenId fingerprint of token
   * @return token detail info
   */
  @Override
  @Transactional(readOnly = true)
  public TokenResponse getTokenDetail(String tokenId) {
    MultiAsset multiAsset = multiAssetRepository.findByFingerprint(tokenId).orElseThrow(
        () -> new BusinessException(BusinessCode.TOKEN_NOT_FOUND)
    );
    String subject = multiAsset.getPolicy() + multiAsset.getName();
    AssetMetadata assetMetadata = assetMetadataRepository.findFirstBySubject(subject).orElse(null);
    TokenResponse response = tokenMapper.fromMultiAssetToResponse(multiAsset);
    response.setMetadata(assetMetadataMapper.fromAssetMetadata(assetMetadata));
    response.setCreatedOn(maTxMintRepository.findCreatedOnByIdent(multiAsset).toLocalDateTime());

    return response;
  }

  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<TokenMintTxResponse> getMintTxs(String tokenId, Pageable pageable) {
    Page<MaTxMint> maTxMints = maTxMintRepository.findByIdent(tokenId, pageable);
    BaseFilterResponse<TokenMintTxResponse> response = new BaseFilterResponse<>();
    response.setData(maTxMints.getContent().stream().map(maTxMintMapper::fromMaTxMintToTokenMintTx)
        .collect(Collectors.toList()));
    response.setTotalItems(maTxMints.getTotalElements());
    response.setTotalPages(maTxMints.getTotalPages());
    response.setCurrentPage(pageable.getPageNumber());
    return response;
  }

  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<TokenAddressResponse> getTopHolders(String tokenId, Pageable pageable) {
    Page<AddressTokenProjection> tokenAddresses = multiAssetRepository.findAddressByToken(tokenId, pageable);
    BaseFilterResponse<TokenAddressResponse> response = new BaseFilterResponse<>();
    response.setData(tokenAddresses.getContent().stream().map(tokenMapper::fromAddressTokenProjection)
        .collect(Collectors.toList()));
    response.setTotalItems(tokenAddresses.getTotalElements());
    response.setTotalPages(tokenAddresses.getTotalPages());
    response.setCurrentPage(pageable.getPageNumber());
    return response;
  }

  @Override
  public BaseFilterResponse<TxFilterResponse> getTxs(String tokenId, Pageable pageable) {
    BaseFilterResponse<TxFilterResponse> response = new BaseFilterResponse<>();
    Optional<MultiAsset> multiAsset = multiAssetRepository.findByFingerprint(tokenId);
    if(multiAsset.isPresent()) {
      List<Long> txIds = addressTokenRepository.findTxsById(multiAsset.get(), pageable);
      List<Tx> txList = txRepository.findByIdInOrderByIdDesc(txIds);
      Set<Long> blockIdList = txList.stream().map(Tx::getBlockId)
          .collect(Collectors.toSet());
      var conditions = Specification.where(BlockSpecification.hasIdIn(blockIdList));
      List<Block> blocks = blockRepository.findAll(conditions);
      Map<Long, Block> mapBlock = blocks.stream()
          .collect(Collectors.toMap(Block::getId, Function.identity()));
      txList.forEach(tx -> tx.setBlock(mapBlock.get(tx.getBlockId())));

      List<TxFilterResponse> txFilterResponses = mapDataFromTxListToResponseList(txList);
      response.setData(txFilterResponses);
      response.setTotalItems(multiAsset.get().getTxCount());
      response.setTotalPages((int) Math.ceil((double) multiAsset.get().getTxCount() / pageable.getPageSize()));
      response.setCurrentPage(pageable.getPageNumber());
    }
    return response;
  }
  /**
   * Mapping from tx entity list to tx response dto
   *
   * @param txList list tx entity
   * @return list tx response
   */
  private List<TxFilterResponse> mapDataFromTxListToResponseList(List<Tx> txList) {

    //get addresses input
    Set<Long> txIdSet = txList.stream().map(Tx::getId).collect(Collectors.toSet());
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
    for (Tx tx : txList) {
      Long txId = tx.getId();
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
}
