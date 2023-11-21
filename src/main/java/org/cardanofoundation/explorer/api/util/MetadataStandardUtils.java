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
import org.cardanofoundation.ledgersync.common.model.BaseProperty;
import org.cardanofoundation.ledgersync.common.model.cip25.MetadataCIP25;
import org.cardanofoundation.ledgersync.common.model.cip25.TokenCIP25;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
@Slf4j
public class MetadataStandardUtils {

  private static final String NAME = "name";
  private static final String IMAGE = "image";
  private static final String MEDIA_TYPE = "mediaType";
  private static final String DESCRIPTION = "description";
  private static final String VERSION = "version";
  private static final String POLICY_ID = "policy_id";
  private static final String ASSET_NAME = "asset_name";
  private static final String SRC = "src";
  private static final String FILES = "files";
  private static final String[] IMAGE_PREFIX = {"http://", "https://", "ipfs://", "ar://"};
  private static final String BASE64_PREFIX = "data:";
  private static final String RAW_BYTE_PREFIX = "0x";
  private static final String BASE64 = "base64";
  private static final String[] MEDIA_TYPE_PREFIX = {"image/", "application/", "audio/", "example/",
      "font/", "model/", "text/", "video/"};

  private static final String[] FIELD_TYPE = {"string", "string | raw bytes", "uri | array",
      "image/<mime_sub_type>", "string | array", "mime_type", "1 or 2", "uri", "array",
      "raw bytes"};


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
  public static MetadataCIP25 metadataStandardCIP25(String jsonMetadata) {
    MetadataCIP25 metadataCIP25 = new MetadataCIP25();
    metadataCIP25.setValid(false);
    Map<Object, TokenCIP25> tokenMap = new HashMap<>();
    try {
      ObjectMapper objectMapper = new ObjectMapper();
      Map<Object, Object> metadataMap = objectMapper.readValue(jsonMetadata, new TypeReference<>() {
      });
      int version = detectVersion(metadataMap.get(VERSION));
      for (Map.Entry<Object, Object> metadataEntry : metadataMap.entrySet()) {
        if (metadataEntry.getValue() instanceof HashMap<?, ?> assetMap) {
          Object policyId = metadataEntry.getKey();
          for (Entry<?, ?> assetEntry : assetMap.entrySet()) {
            TokenCIP25 token = new TokenCIP25();
            Map<Object, Object> assetValMap = (Map<Object, Object>) assetEntry.getValue();
            List<BaseProperty> requireProperties = new ArrayList<>();
            requireProperties.add(policyMetadataCIP25(policyId, version));
            requireProperties.add(assetNameMetadataCIP25(assetEntry.getKey(), version));
            requireProperties.add(nameMetadataCIP25(assetValMap.get(NAME), version));
            requireProperties.add(imageMetadataCIP25(assetValMap.get(IMAGE), version));
            requireProperties.sort(Comparator.comparing(BaseProperty::getIndex));
            token.setRequireProperties(requireProperties);
            List<BaseProperty> optionalProperties = new ArrayList<>();
            Object desc = assetValMap.get(DESCRIPTION);
            if (Objects.nonNull(desc)) {
              optionalProperties.add(descriptionMetadataCIP25(desc, version));
            }
            Object mediaType = assetValMap.get(MEDIA_TYPE);
            if (Objects.nonNull(mediaType)) {
              optionalProperties.add(mediaTypeMetadataCIP25(mediaType, version));
            }
            filesMetadataCIP25(assetValMap.get(FILES), optionalProperties, version);
            versionMetadataCIP25(metadataMap.get(VERSION), optionalProperties);
            optionalProperties.sort(Comparator.comparing(BaseProperty::getIndex));
            token.setOptionalProperties(optionalProperties);
            token.setTokenName(assetEntry.getKey());
            tokenMap.put(assetEntry.getKey(), token);
          }
        }
      }
      metadataCIP25.setValid(validCIP25(tokenMap, version));
    } catch (Exception ex) {
      log.error("Error: structure incorrect, message=" + ex.getMessage());
      log.error("Check standard CIP-25 fail");
    }
    metadataCIP25.setTokenMap(tokenMap);
    return metadataCIP25;
  }

  public static void versionMetadataCIP25(Object version, List<BaseProperty> optionalProperties) {
    if (Objects.isNull(version)) {
      return;
    }
    optionalProperties.add(
        BaseProperty.builder().value(version).format(FIELD_TYPE[6]).property(VERSION).index(
                String.valueOf(optionalProperties.stream()
                    .filter(optionalProp -> optionalProp.getIndex().length() == 1).count() + 1))
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

  public static BaseProperty policyMetadataCIP25(Object policyId, int version) {
    BaseProperty baseProperty = BaseProperty.builder().value(policyId).format(FIELD_TYPE[1])
        .index("1")
        .property(POLICY_ID)
        .valid(false).build();
    switch (version) {
      case 0 -> {
        baseProperty.setValid(null);
        baseProperty.setFormat(null);
      }
      case 1 -> {
        if (Objects.nonNull(policyId) && policyId instanceof String policyIdStr) {
          baseProperty.setValid(!hexString(policyIdStr));
          baseProperty.setFormat(FIELD_TYPE[0]);
        }
      }
      case 2 -> {
        if (Objects.nonNull(policyId) && policyId instanceof String policyIdStr) {
          baseProperty.setValid(hexString(policyIdStr));
          baseProperty.setFormat(FIELD_TYPE[9]);
        }
      }
      default -> log.warn("version is not define");
    }
    return baseProperty;
  }

  public static BaseProperty assetNameMetadataCIP25(Object assetName, int version) {
    BaseProperty baseProperty = BaseProperty.builder().value(assetName).format(FIELD_TYPE[1])
        .index("2")
        .property(ASSET_NAME)
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
          baseProperty.setFormat(FIELD_TYPE[0]);
        }
      }
      case 2 -> {
        if (Objects.nonNull(assetName) && assetName instanceof String assetNameStr) {
          baseProperty.setValid(hexString(assetNameStr));
          baseProperty.setFormat(FIELD_TYPE[9]);
        }
      }
      default -> log.warn("version is not define");
    }
    return baseProperty;
  }

  public static BaseProperty nameMetadataCIP25(Object name, int version) {
    BaseProperty baseProperty = BaseProperty.builder().value(name).format(FIELD_TYPE[0])
        .property(NAME)
        .index("3")
        .valid(Objects.nonNull(name) && name instanceof String)
        .build();
    if (version == 0) {
      baseProperty.setValid(null);
      baseProperty.setFormat(null);
    }
    return baseProperty;
  }

  public static BaseProperty nameFileMetadataCIP25(Object name, int version) {
    BaseProperty baseProperty = BaseProperty.builder().value(name).format(FIELD_TYPE[0])
        .property(NAME)
        .valid(Objects.nonNull(name) && name instanceof String)
        .build();
    if (version == 0) {
      baseProperty.setValid(null);
      baseProperty.setFormat(null);
    }
    return baseProperty;
  }

  public static BaseProperty imageMetadataCIP25(Object image, int version) {
    BaseProperty checkImage = BaseProperty.builder().value(image).format(FIELD_TYPE[2])
        .index("4")
        .property(IMAGE).valid(false).build();
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

  public static BaseProperty srcFileMetadataCIP25(Object src, int version) {
    BaseProperty checkSrc = BaseProperty.builder().value(src).valid(false).format(FIELD_TYPE[2])
        .property(SRC).build();
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

  public static BaseProperty mediaTypeMetadataCIP25(Object mediaType, int version) {
    BaseProperty baseProperty = BaseProperty.builder().value(mediaType).format(FIELD_TYPE[3])
        .property(MEDIA_TYPE)
        .valid(mediaType instanceof String mediaTypeStr && mediaTypeStr.startsWith(
            MEDIA_TYPE_PREFIX[0])).build();
    if (version == 0) {
      baseProperty.setValid(null);
      baseProperty.setFormat(null);
    }
    return baseProperty;
  }

  public static BaseProperty mediaTypeFileMetadataCIP25(Object mediaType, int version) {
    BaseProperty baseProperty = BaseProperty.builder().value(mediaType).format(FIELD_TYPE[5])
        .property(MEDIA_TYPE)
        .valid(
            Objects.nonNull(mediaType) && mediaType instanceof String mediaTypeStr && Arrays.stream(
                MEDIA_TYPE_PREFIX).anyMatch(mediaTypeStr::startsWith)).build();
    if (version == 0) {
      baseProperty.setValid(null);
      baseProperty.setFormat(null);
    }
    return baseProperty;
  }

  public static BaseProperty descriptionMetadataCIP25(Object desc, int version) {
    BaseProperty baseProperty = BaseProperty.builder().value(desc)
        .format(FIELD_TYPE[4])
        .property(DESCRIPTION)
        .valid(Objects.isNull(desc) || desc instanceof String || desc instanceof ArrayList<?>)
        .build();
    if (version == 0) {
      baseProperty.setValid(null);
      baseProperty.setFormat(null);
    }
    return baseProperty;
  }

  public static void filesMetadataCIP25(Object files, List<BaseProperty> optionalProperties,
      int version) {
    if (Objects.nonNull(files) && files instanceof ArrayList<?> fileList && !fileList.isEmpty()) {
      BaseProperty filesProperty = BaseProperty.builder().valid(false)
          .property(FILES).valid(true)
          .format(FIELD_TYPE[8])
          .build();
      List<Boolean> validFiles = new ArrayList<>();
      int indexInFile = 1;
      int indexOfFile = optionalProperties.size() + 1;
      for (Object file : fileList) {
        if (file instanceof HashMap<?, ?> fileMap) {
          String index = indexOfFile + "." + indexInFile;
          BaseProperty nameFile = nameFileMetadataCIP25(fileMap.get(NAME), version);
          nameFile.setIndex(index + ".1");
          validFiles.add(nameFile.getValid());
          optionalProperties.add(nameFile);
          BaseProperty srcFile = srcFileMetadataCIP25(fileMap.get(SRC), version);
          srcFile.setIndex(index + ".2");
          validFiles.add(srcFile.getValid());
          optionalProperties.add(srcFile);
          BaseProperty mediaTypeFile = mediaTypeFileMetadataCIP25(fileMap.get(MEDIA_TYPE), version);
          mediaTypeFile.setIndex(index + ".3");
          validFiles.add(mediaTypeFile.getValid());
          optionalProperties.add(mediaTypeFile);
          indexInFile++;
        }
      }
      if (version == 0) {
        filesProperty.setValid(null);
        filesProperty.setFormat(null);
      } else {
        filesProperty.setValid(
            !validFiles.isEmpty() && validFiles.stream().allMatch(valid -> valid.equals(true)));
      }
      filesProperty.setIndex(String.valueOf(indexOfFile));
      optionalProperties.add(filesProperty);
    }
    int index = 1;
    for (BaseProperty baseProperty : optionalProperties) {
      if (Objects.isNull(baseProperty.getIndex())) {
        baseProperty.setIndex(String.valueOf(index));
        index++;
      }
    }
  }

  public static Boolean validCIP25(Map<Object, TokenCIP25> tokenMap, int version) {
    if (version == 0) {
      return false;
    }
    List<Boolean> fields = new ArrayList<>();
    for (Map.Entry<Object, TokenCIP25> tokenEntry : tokenMap.entrySet()) {
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
