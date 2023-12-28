package org.cardanofoundation.explorer.api.util;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.*;
import java.util.Map.Entry;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.cardanofoundation.explorer.api.common.enumeration.FormatFieldType;
import org.cardanofoundation.explorer.api.common.enumeration.MetadataField;
import org.cardanofoundation.explorer.api.model.metadatastandard.BaseProperty;
import org.cardanofoundation.explorer.api.model.metadatastandard.cip.MetadataCIP;
import org.cardanofoundation.explorer.api.model.metadatastandard.cip.TokenCIP;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
@Slf4j
public class MetadataCIP25Utils {

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
    boolean isValid = version instanceof Integer versionInt && (versionInt == 1 || versionInt == 2);

    optionalProperties.add(
        BaseProperty.builder().value(version)
            .format(FormatFieldType.VERSION_1_OR_2.getValue())
            .property(MetadataField.VERSION.getName()).index(index)
            .valid(isValid)
            .valueFormat(isValid ? version.toString()
                                 : FormatFieldType.NEITHER_VERSION_1_OR_2.getValue())
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
    FormatFieldType format = MetadataFieldUtils.getFormatTypeByObject(policyId);
    BaseProperty baseProperty = BaseProperty.builder().value(policyId)
        .format(FormatFieldType.STRING_OR_RAW_BYTES.getValue())
        .index("1")
        .property(MetadataField.POLICY_ID.getName())
        .valueFormat(format.getValue())
        .valid(false).build();
    switch (version) {
      case 0 -> {
        baseProperty.setValid(null);
        baseProperty.setFormat(null);
        baseProperty.setValueFormat(null);
      }
      case 1 -> baseProperty.setValid(format.equals(FormatFieldType.STRING));
      case 2 -> baseProperty.setValid(format.equals(FormatFieldType.RAW_BYTES));
      default -> log.warn("version is not define");
    }
    return baseProperty;
  }

  public static BaseProperty assetName(Object assetName, int version) {
    FormatFieldType format = MetadataFieldUtils.getFormatTypeByObject(assetName);
    BaseProperty baseProperty = BaseProperty.builder().value(assetName)
        .format(FormatFieldType.STRING_OR_RAW_BYTES.getValue())
        .index("2")
        .property(MetadataField.ASSET_NAME.getName())
        .valueFormat(format.getValue())
        .valid(false).build();
    switch (version) {
      case 0 -> {
        baseProperty.setValid(null);
        baseProperty.setFormat(null);
        baseProperty.setValueFormat(null);
      }
      case 1 -> baseProperty.setValid(format.equals(FormatFieldType.STRING));
      case 2 -> baseProperty.setValid(format.equals(FormatFieldType.RAW_BYTES));
      default -> log.warn("version is not define");
    }
    return baseProperty;
  }

  public static BaseProperty name(Object name, int version) {
    FormatFieldType format = MetadataFieldUtils.getFormatTypeByObject(name);
    BaseProperty baseProperty = BaseProperty.builder().value(name)
        .format(FormatFieldType.STRING.getValue())
        .property(MetadataField.NAME.getName())
        .index("3")
        .valid(format.equals(FormatFieldType.STRING))
        .valueFormat(format.getValue())
        .build();
    if (version == 0) {
      baseProperty.setValid(null);
      baseProperty.setFormat(null);
      baseProperty.setValueFormat(null);
    }
    return baseProperty;
  }

  public static BaseProperty nameFile(Object name, int version) {
    FormatFieldType format = MetadataFieldUtils.getFormatTypeByObject(name);
    BaseProperty baseProperty = BaseProperty.builder().value(name)
        .format(FormatFieldType.STRING.getValue())
        .property(MetadataField.NAME.getName())
        .valid(format.equals(FormatFieldType.STRING))
        .valueFormat(format.getValue())
        .build();
    if (version == 0) {
      baseProperty.setValid(null);
      baseProperty.setFormat(null);
      baseProperty.setValueFormat(null);
    }
    return baseProperty;
  }

  public static BaseProperty image(Object image, int version) {
    FormatFieldType format = MetadataFieldUtils.getFormatTypeByObject(image);
    BaseProperty checkImage = BaseProperty.builder().value(image)
        .format(FormatFieldType.URI_OR_ARRAY.getValue())
        .index("4")
        .valueFormat(format.getValue())
        .valid(format.equals(FormatFieldType.URI) || format.equals(FormatFieldType.URI_ARRAY_PARTS))
        .property(MetadataField.IMAGE.getName())
        .build();

    if (version == 0) {
      checkImage.setValid(null);
      checkImage.setFormat(null);
      checkImage.setValueFormat(null);
    }
    return checkImage;
  }

  public static BaseProperty srcFile(Object src, int version) {
    FormatFieldType format = MetadataFieldUtils.getFormatTypeByObject(src);
    BaseProperty checkSrc = BaseProperty.builder().value(src)
        .format(FormatFieldType.URI_OR_ARRAY.getValue())
        .property(MetadataField.SRC.getName())
        .valueFormat(format.getValue())
        .valid(format.equals(FormatFieldType.URI) || format.equals(FormatFieldType.URI_ARRAY_PARTS))
        .build();

    if (version == 0) {
      checkSrc.setValid(null);
      checkSrc.setFormat(null);
      checkSrc.setValueFormat(null);
    }
    return checkSrc;
  }

  public static BaseProperty mediaType(Object mediaType, String index, int version) {
    FormatFieldType format = MetadataFieldUtils.getFormatTypeByObject(mediaType);
    boolean isValid = format.equals(FormatFieldType.IMAGE_SLASH_MIME_SUB_TYPE);
    BaseProperty baseProperty = BaseProperty.builder()
        .value(mediaType)
        .format(FormatFieldType.IMAGE_SLASH_MIME_SUB_TYPE.getValue())
        .property(MetadataField.MEDIA_TYPE.getName())
        .index(index)
        .valueFormat(isValid ? mediaType.toString() : format.getValue())
        .valid(isValid)
        .build();

    if (version == 0) {
      baseProperty.setValid(null);
      baseProperty.setFormat(null);
      baseProperty.setValueFormat(null);
    }
    return baseProperty;
  }

  public static BaseProperty mediaTypeFile(Object mediaType, int version) {
    FormatFieldType format = MetadataFieldUtils.getFormatTypeByObject(mediaType);
    boolean isValid = format.equals(FormatFieldType.IMAGE_SLASH_MIME_SUB_TYPE)
        || format.equals(FormatFieldType.MIME_TYPE);
    BaseProperty baseProperty = BaseProperty.builder().value(mediaType)
        .format(FormatFieldType.MIME_TYPE.getValue())
        .property(MetadataField.MEDIA_TYPE.getName())
        .valueFormat(isValid ? mediaType.toString() : format.getValue())
        .valid(isValid).build();
    if (version == 0) {
      baseProperty.setValid(null);
      baseProperty.setFormat(null);
      baseProperty.setValueFormat(null);
    }
    return baseProperty;
  }

  public static BaseProperty description(Object desc, String index, int version) {
    FormatFieldType format = MetadataFieldUtils.getFormatTypeByObject(desc);
    BaseProperty baseProperty = BaseProperty.builder().value(desc)
        .format(FormatFieldType.STRING_OR_ARRAY_STRING.getValue())
        .property(MetadataField.DESCRIPTION.getName())
        .index(index)
        .valueFormat(format.getValue())
        .valid(format.equals(FormatFieldType.STRING) || format.equals(FormatFieldType.ARRAY))
        .build();
    if (version == 0) {
      baseProperty.setValid(null);
      baseProperty.setFormat(null);
      baseProperty.setValueFormat(null);
    }
    return baseProperty;
  }

  public static int files(Object files, List<BaseProperty> optionalProperties, int indexOfFile,
      int version) {
    if (Objects.nonNull(files) && files instanceof ArrayList<?> fileList && !fileList.isEmpty()) {
      BaseProperty filesProperty = BaseProperty.builder().valid(false)
          .property(MetadataField.FILES.getName()).valid(true)
          .format(FormatFieldType.ARRAY.getValue())
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
                // The validation of "name" property won't affect the validation of "files" optional property
                .filter(baseProperty -> Objects.nonNull(baseProperty) &&
                        !baseProperty.getProperty().equals(MetadataField.NAME.getName()))
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
      List<BaseProperty> optionalProperties = tokenEntry.getValue().getOptionalProperties();

      // Find the index of the highest level "files" property (since "files" is an optional property this can be an empty optional)
      Optional<String> filesIndex = optionalProperties.stream().filter(baseProperty -> {
          String index = baseProperty.getIndex();
          return index.matches("^[\\d]+") && baseProperty.getProperty().equals(MetadataField.FILES.getName());
      }).findFirst().map(BaseProperty::getIndex);

      // The validation of files[<index>]."name" property of a token won't affect the validation of the tokens
      if (filesIndex.isPresent()){
         optionalProperties = optionalProperties.stream()
                    .filter(baseProperty -> Objects.nonNull(baseProperty) &&
                            !(baseProperty.getProperty().equals(MetadataField.NAME.getName()) && baseProperty.getIndex().matches(filesIndex.get() + "\\.\\d+\\.1"))
                    ).toList();
      }

      fields.add(
              optionalProperties.stream().map(BaseProperty::getValid)
              .allMatch(isValid -> isValid.equals(true)));
      fields.add(
          tokenEntry.getValue().getRequireProperties().stream().map(BaseProperty::getValid)
              .allMatch(isValid -> isValid.equals(true)));
    }
    return !fields.isEmpty() && fields.stream().allMatch(field -> field.equals(true));
  }
}
