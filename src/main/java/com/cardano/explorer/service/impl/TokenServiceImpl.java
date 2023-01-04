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
import com.cardano.explorer.repository.MaTxMintRepository;
import com.cardano.explorer.repository.MultiAssetRepository;
import com.cardano.explorer.service.TokenService;
import com.sotatek.cardano.common.entity.MaTxMint;
import com.sotatek.cardano.common.entity.MultiAsset;
import com.sotatek.cardanocommonapi.exceptions.BusinessException;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
@Log4j2
public class TokenServiceImpl implements TokenService {

  private final MultiAssetRepository multiAssetRepository;
  private final MaTxMintRepository maTxMintRepository;
  private final TokenMapper tokenMapper;
  private final MaTxMintMapper maTxMintMapper;


  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<TokenFilterResponse> filterToken(Pageable pageable) {
    Page<MultiAsset> multiAssets = multiAssetRepository.findAll(pageable);
    return new BaseFilterResponse<>(multiAssets.map(tokenMapper::fromMultiAssetToFilterResponse));
  }


  @Override
  @Transactional(readOnly = true)
  public TokenResponse getTokenDetail(String tokenId) {
    MultiAsset multiAsset = multiAssetRepository.findByFingerprint(tokenId).orElseThrow(
        () -> new BusinessException(BusinessCode.TOKEN_NOT_FOUND)
    );
    return tokenMapper.fromMultiAssetToResponse(multiAsset);
  }

  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<TokenMintTxResponse> getMintTxs(String tokenId, Pageable pageable) {
    Page<MaTxMint> maTxMints = maTxMintRepository.findByIdent(tokenId, pageable);
    return new BaseFilterResponse<>(maTxMints.map(maTxMintMapper::fromMaTxMintToTokenMintTx));
  }

  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<TokenAddressResponse> getTopHolders(String tokenId, Pageable pageable) {
    Page<AddressTokenProjection> tokenAddresses = multiAssetRepository.findAddressByToken(tokenId, pageable);
    return new BaseFilterResponse<>(tokenAddresses.map(tokenMapper::fromAddressTokenProjection));
  }
}
