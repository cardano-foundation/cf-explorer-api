package org.cardanofoundation.explorer.api.util;

import com.bloxbean.cardano.client.util.HexUtil;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.cardanofoundation.explorer.api.common.enumeration.MetadataField;
import org.cardanofoundation.explorer.api.model.metadatastandard.BaseProperty;
import org.cardanofoundation.explorer.api.model.metadatastandard.cip.MetadataCIP;
import org.cardanofoundation.explorer.api.model.metadatastandard.cip.TokenCIP;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
@Slf4j
public class MetadataCIP25Utils {

  private static final String[] IMAGE_PREFIX = {"http://", "https://", "ipfs://", "ar://"};
  private static final String BASE64_PREFIX = "data:";
  private static final String RAW_BYTE_PREFIX = "0x";
  private static final String BASE64 = "base64";
  private static final String[] MEDIA_TYPE_PREFIX = {"image/", "application/", "audio/", "example/",
      "font/", "model/", "text/", "video/"};

  public static String splitJsonMetadataByAssetName(String jsonMetadata, String assetName) {
    try {
      ObjectMapper objectMapper = new ObjectMapper();
      Map<Object, Object> metadataMap = objectMapper.readValue(jsonMetadata, new TypeReference<>() {
      });
      Map<Object, Object> finalMap = new HashMap<>();
      boolean isAsset = false;
      for (Map.Entry<Object, Object> metadataEntry : metadataMap.entrySet()) {
        Object key = metadataEntry.getKey();
        Object val = metadataEntry.getValue();
        if (val instanceof HashMap<?, ?> assetMap) {
          for (Entry<?, ?> assetEntry : assetMap.entrySet()) {
            Object assetKey = assetEntry.getKey();
            if (assetKey instanceof String assetKeyStr && (
                assetKeyStr.replaceAll("\\s+", "").equals(assetName.replaceAll("\\s+", ""))
                    || (assetKeyStr.startsWith("0x") && assetKeyStr.replace("0x", "")
                    .equals(assetName)))) {
              isAsset = true;
              Map<Object, Object> subMap = new HashMap<>();
              subMap.put(assetKey, assetEntry.getValue());
              finalMap.put(key, subMap);
            }
          }
        } else {
          finalMap.put(key, val);
        }
      }
      return isAsset ? objectMapper.writeValueAsString(finalMap) : jsonMetadata;
    } catch (Exception ex) {
      log.error("Error: structure incorrect, message=" + ex.getMessage());
      log.error("Split Json metadata fail");
    }
    return jsonMetadata;
  }

  @SuppressWarnings("unchecked")
  public static MetadataCIP standard(String jsonMetadata) {
    MetadataCIP metadataCIP = new MetadataCIP();
    metadataCIP.setValid(false);
    Map<Object, TokenCIP> tokenMap = new HashMap<>();
    try {
      ObjectMapper objectMapper = new ObjectMapper();
      Map<Object, Object> metadataMap = objectMapper.readValue(jsonMetadata, new TypeReference<>() {
      });
      int version = detectVersion(metadataMap.get(MetadataField.VERSION.getName()));
      for (Map.Entry<Object, Object> metadataEntry : metadataMap.entrySet()) {
        if (metadataEntry.getValue() instanceof HashMap<?, ?> assetMap) {
          Object policyId = metadataEntry.getKey();
          for (Entry<?, ?> assetEntry : assetMap.entrySet()) {
            TokenCIP token = new TokenCIP();
            Map<Object, Object> assetValMap = (Map<Object, Object>) assetEntry.getValue();
            List<BaseProperty> requireProperties = new ArrayList<>();
            requireProperties.add(policy(policyId, version));
            requireProperties.add(assetName(assetEntry.getKey(), version));
            requireProperties.add(
                name(assetValMap.get(MetadataField.NAME.getName()), version));
            requireProperties.add(
                image(assetValMap.get(MetadataField.IMAGE.getName()), version));
            requireProperties.sort(Comparator.comparing(BaseProperty::getIndex));
            token.setRequireProperties(requireProperties);
            List<BaseProperty> optionalProperties = new ArrayList<>();
            Object desc = assetValMap.get(MetadataField.DESCRIPTION.getName());
            int index = 1;
            if (Objects.nonNull(desc)) {
              optionalProperties.add(description(desc, String.valueOf(index), version));
              index++;
            }
            Object mediaType = assetValMap.get(MetadataField.MEDIA_TYPE.getName());
            if (Objects.nonNull(mediaType)) {
              optionalProperties.add(mediaType(mediaType, String.valueOf(index), version));
              index++;
            }
            index = files(assetValMap.get(MetadataField.FILES.getName()), optionalProperties, index,
                version);
            version(metadataMap.get(MetadataField.VERSION.getName()), String.valueOf(index),
                optionalProperties);
            optionalProperties.sort(Comparator.comparing(BaseProperty::getIndex));
            token.setOptionalProperties(optionalProperties);
            token.setTokenName(assetEntry.getKey());
            tokenMap.put(assetEntry.getKey(), token);
          }
        }
      }
      metadataCIP.setValid(valid(tokenMap, version));
    } catch (Exception ex) {
      log.error("Error: structure incorrect, message=" + ex.getMessage());
      log.error("Check standard CIP-25 fail");
    }
    metadataCIP.setTokenMap(tokenMap);
    return metadataCIP;
  }

  public static void version(Object version, String index, List<BaseProperty> optionalProperties) {
    if (Objects.isNull(version)) {
      return;
    }
    optionalProperties.add(
        BaseProperty.builder().value(version).format(CommonConstant.FIELD_TYPE[6])
            .property(MetadataField.VERSION.getName()).index(index)
            .valid(version instanceof Integer versionInt && (versionInt == 1 || versionInt == 2))
            .build());
  }

  public static int detectVersion(Object version) {
    if (Objects.isNull(version) || (version instanceof Integer versionInt && versionInt == 1)) {
      return 1;
    } else if (version instanceof Integer versionInt && versionInt == 2) {
      return 2;
    } else {
      return 0;
    }
  }

  public static BaseProperty policy(Object policyId, int version) {
    BaseProperty baseProperty = BaseProperty.builder().value(policyId)
        .format(CommonConstant.FIELD_TYPE[1])
        .index("1")
        .property(MetadataField.POLICY_ID.getName())
        .valid(false).build();
    switch (version) {
      case 0 -> {
        baseProperty.setValid(null);
        baseProperty.setFormat(null);
      }
      case 1 -> {
        if (Objects.nonNull(policyId) && policyId instanceof String policyIdStr) {
          baseProperty.setValid(!hexString(policyIdStr));
          baseProperty.setFormat(CommonConstant.FIELD_TYPE[0]);
        }
      }
      case 2 -> {
        if (Objects.nonNull(policyId) && policyId instanceof String policyIdStr) {
          baseProperty.setValid(hexString(policyIdStr));
          baseProperty.setFormat(CommonConstant.FIELD_TYPE[9]);
        }
      }
      default -> log.warn("version is not define");
    }
    return baseProperty;
  }

  public static BaseProperty assetName(Object assetName, int version) {
    BaseProperty baseProperty = BaseProperty.builder().value(assetName)
        .format(CommonConstant.FIELD_TYPE[1])
        .index("2")
        .property(MetadataField.ASSET_NAME.getName())
        .valid(false).build();
    switch (version) {
      case 0 -> {
        baseProperty.setValid(null);
        baseProperty.setFormat(null);
      }
      case 1 -> {
        if (Objects.nonNull(assetName) && assetName instanceof String assetNameStr) {
          baseProperty.setValid(!hexString(assetNameStr) && StandardCharsets.UTF_8.newEncoder()
              .canEncode(assetNameStr));
          baseProperty.setFormat(CommonConstant.FIELD_TYPE[0]);
        }
      }
      case 2 -> {
        if (Objects.nonNull(assetName) && assetName instanceof String assetNameStr) {
          baseProperty.setValid(hexString(assetNameStr));
          baseProperty.setFormat(CommonConstant.FIELD_TYPE[9]);
        }
      }
      default -> log.warn("version is not define");
    }
    return baseProperty;
  }

  public static BaseProperty name(Object name, int version) {
    BaseProperty baseProperty = BaseProperty.builder().value(name)
        .format(CommonConstant.FIELD_TYPE[0])
        .property(MetadataField.NAME.getName())
        .index("3")
        .valid(Objects.nonNull(name) && name instanceof String)
        .build();
    if (version == 0) {
      baseProperty.setValid(null);
      baseProperty.setFormat(null);
    }
    return baseProperty;
  }

  public static BaseProperty nameFile(Object name, int version) {
    BaseProperty baseProperty = BaseProperty.builder().value(name)
        .format(CommonConstant.FIELD_TYPE[0])
        .property(MetadataField.NAME.getName())
        .valid(Objects.nonNull(name) && name instanceof String)
        .build();
    if (version == 0) {
      baseProperty.setValid(null);
      baseProperty.setFormat(null);
    }
    return baseProperty;
  }

  public static BaseProperty image(Object image, int version) {
    BaseProperty checkImage = BaseProperty.builder().value(image)
        .format(CommonConstant.FIELD_TYPE[2])
        .index("4")
        .property(MetadataField.IMAGE.getName()).valid(false).build();
    if (Objects.nonNull(image) && image instanceof String imageStr && (
        Arrays.stream(IMAGE_PREFIX).anyMatch(imageStr::startsWith) || (
            imageStr.startsWith(BASE64_PREFIX) && imageStr.contains(BASE64)))) {
      checkImage.setValid(true);
    } else if (Objects.nonNull(image) && image instanceof ArrayList<?> imageList
        && imageList.size() >= 2) {
      checkImage.setValid(
          Arrays.stream(IMAGE_PREFIX).anyMatch(imageList.get(0).toString()::startsWith));
    }
    if (version == 0) {
      checkImage.setValid(null);
      checkImage.setFormat(null);
    }
    return checkImage;
  }

  public static BaseProperty srcFile(Object src, int version) {
    BaseProperty checkSrc = BaseProperty.builder().value(src).valid(false)
        .format(CommonConstant.FIELD_TYPE[2])
        .property(MetadataField.SRC.getName()).build();
    if (Objects.nonNull(src) && src instanceof String imageStr && Arrays.stream(IMAGE_PREFIX)
        .anyMatch(imageStr::startsWith)) {
      checkSrc.setValid(true);
    } else if (Objects.nonNull(src) && src instanceof ArrayList<?> imageList
        && imageList.size() >= 2) {
      checkSrc.setValid(
          Arrays.stream(IMAGE_PREFIX).anyMatch(imageList.get(0).toString()::startsWith));
    }
    if (version == 0) {
      checkSrc.setValid(null);
      checkSrc.setFormat(null);
    }
    return checkSrc;
  }

  public static BaseProperty mediaType(Object mediaType, String index, int version) {
    BaseProperty baseProperty = BaseProperty.builder().value(mediaType)
        .format(CommonConstant.FIELD_TYPE[3])
        .property(MetadataField.MEDIA_TYPE.getName())
        .index(index)
        .valid(mediaType instanceof String mediaTypeStr && mediaTypeStr.startsWith(
            MEDIA_TYPE_PREFIX[0])).build();
    if (version == 0) {
      baseProperty.setValid(null);
      baseProperty.setFormat(null);
    }
    return baseProperty;
  }

  public static BaseProperty mediaTypeFile(Object mediaType, int version) {
    BaseProperty baseProperty = BaseProperty.builder().value(mediaType)
        .format(CommonConstant.FIELD_TYPE[5])
        .property(MetadataField.MEDIA_TYPE.getName())
        .valid(
            Objects.nonNull(mediaType) && mediaType instanceof String mediaTypeStr && Arrays.stream(
                MEDIA_TYPE_PREFIX).anyMatch(mediaTypeStr::startsWith)).build();
    if (version == 0) {
      baseProperty.setValid(null);
      baseProperty.setFormat(null);
    }
    return baseProperty;
  }

  public static BaseProperty description(Object desc, String index, int version) {
    BaseProperty baseProperty = BaseProperty.builder().value(desc)
        .format(CommonConstant.FIELD_TYPE[4])
        .property(MetadataField.DESCRIPTION.getName())
        .index(index)
        .valid(Objects.isNull(desc) || desc instanceof String || desc instanceof ArrayList<?>)
        .build();
    if (version == 0) {
      baseProperty.setValid(null);
      baseProperty.setFormat(null);
    }
    return baseProperty;
  }

  public static int files(Object files, List<BaseProperty> optionalProperties, int indexOfFile,
      int version) {
    if (Objects.nonNull(files) && files instanceof ArrayList<?> fileList && !fileList.isEmpty()) {
      BaseProperty filesProperty = BaseProperty.builder().valid(false)
          .property(MetadataField.FILES.getName()).valid(true)
          .format(CommonConstant.FIELD_TYPE[8])
          .build();
      List<BaseProperty> optionalPropertiesInFile = new ArrayList<>();
      int indexInFile = 1;
      for (Object file : fileList) {
        if (file instanceof HashMap<?, ?> fileMap) {
          String index = indexOfFile + "." + indexInFile;
          BaseProperty nameFile = nameFile(fileMap.get(MetadataField.NAME.getName()),
              version);
          nameFile.setIndex(index + ".1");
          optionalPropertiesInFile.add(nameFile);
          BaseProperty srcFile = srcFile(fileMap.get(MetadataField.SRC.getName()),
              version);
          srcFile.setIndex(index + ".2");
          optionalPropertiesInFile.add(srcFile);
          BaseProperty mediaTypeFile = mediaTypeFile(
              fileMap.get(MetadataField.MEDIA_TYPE.getName()), version);
          mediaTypeFile.setIndex(index + ".3");
          optionalPropertiesInFile.add(mediaTypeFile);
          indexInFile++;
        }
      }
      if (version == 0) {
        filesProperty.setValid(null);
        filesProperty.setFormat(null);
      } else {
        filesProperty.setValid(
            !optionalPropertiesInFile.isEmpty() && optionalPropertiesInFile.stream()
                .allMatch(baseProperty -> baseProperty.getValid().equals(true)));
      }
      filesProperty.setIndex(String.valueOf(indexOfFile));
      optionalProperties.add(filesProperty);
      optionalProperties.addAll(optionalPropertiesInFile);
      indexOfFile++;
    }
    return indexOfFile;
  }

  public static Boolean valid(Map<Object, TokenCIP> tokenMap, int version) {
    if (version == 0) {
      return false;
    }
    List<Boolean> fields = new ArrayList<>();
    for (Map.Entry<Object, TokenCIP> tokenEntry : tokenMap.entrySet()) {
      fields.add(
          tokenEntry.getValue().getOptionalProperties().stream().map(BaseProperty::getValid)
              .allMatch(isValid -> isValid.equals(true)));
      fields.add(
          tokenEntry.getValue().getRequireProperties().stream().map(BaseProperty::getValid)
              .allMatch(isValid -> isValid.equals(true)));
    }
    return !fields.isEmpty() && fields.stream().allMatch(field -> field.equals(true));
  }

  public static boolean hexString(String str) {
    try {
      HexUtil.decodeHexString(str);
      return str.startsWith(RAW_BYTE_PREFIX);
    } catch (Exception ex) {
      log.error("String is not raw bytes");
      log.error("Error: " + ex.getMessage());
    }
    return false;
  }
}
