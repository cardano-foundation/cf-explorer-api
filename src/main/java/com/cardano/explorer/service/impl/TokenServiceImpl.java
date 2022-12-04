package com.cardano.explorer.service.impl;

import com.cardano.explorer.exception.BusinessCode;
import com.cardano.explorer.mapper.MaTxMintMapper;
import com.cardano.explorer.mapper.TokenMapper;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.token.TokenAddressResponse;
import com.cardano.explorer.model.response.token.TokenFilterResponse;
import com.cardano.explorer.model.response.token.TokenMintTxResponse;
import com.cardano.explorer.model.response.token.TokenResponse;
import com.cardano.explorer.projection.AddressTokenProjection;
import com.cardano.explorer.projection.TokenProjection;
import com.cardano.explorer.repository.MaTxMintRepository;
import com.cardano.explorer.repository.MultiAssetRepository;
import com.cardano.explorer.service.TokenService;
import com.sotatek.cardano.common.entity.MaTxMint;
import com.sotatek.cardano.common.entity.MultiAsset;
import com.sotatek.cardanocommonapi.exceptions.BusinessException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
public class TokenServiceImpl implements TokenService {

  private final MultiAssetRepository multiAssetRepository;
  private final MaTxMintRepository maTxMintRepository;
  private final TokenMapper tokenMapper;
  private final MaTxMintMapper maTxMintMapper;

  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<TokenFilterResponse> filterToken(Pageable pageable) {
    BaseFilterResponse<TokenFilterResponse> response = new BaseFilterResponse<>();
    Page<MultiAsset> multiAssets = multiAssetRepository.findAll(pageable);
    List<TokenFilterResponse> tokenFilterResponses = new ArrayList<>();
    List<TokenProjection> supplyList = maTxMintRepository.findCreatedOnByIdentIn(multiAssets.toList());
    Map<Long, TokenProjection> supplyMap = supplyList.stream().collect(Collectors.toMap(
        TokenProjection::getIdent, Function.identity()
    ));
    multiAssets.forEach(
        multiAsset -> {
          TokenFilterResponse token = tokenMapper.fromMultiAssetToFilterResponse(multiAsset);
          token.setCreatedOn(supplyMap.get(multiAsset.getId()).getCreatedOn().toLocalDateTime());
          tokenFilterResponses.add(token);
        }
    );
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
    TokenResponse response = tokenMapper.fromMultiAssetToResponse(multiAsset);
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
}
