package org.cardanofoundation.explorer.api.mapper;
import org.cardanofoundation.explorer.api.model.response.token.TokenMetadataResponse;
import com.sotatek.cardano.common.entity.AssetMetadata;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface AssetMetadataMapper {

  TokenMetadataResponse fromAssetMetadata(AssetMetadata metadata);
}
