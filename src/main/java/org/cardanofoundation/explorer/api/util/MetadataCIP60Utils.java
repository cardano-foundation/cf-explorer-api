package org.cardanofoundation.explorer.api.util;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.ArrayList;
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
public class MetadataCIP60Utils {

  private static final String SONG_DURATION_REGEX = "^P(?!$)(T(?=\\d)(\\d+H)?(\\d+M)?(\\d+S)?)$";

  @SuppressWarnings("unchecked")
  public static MetadataCIP standard(String jsonMetadata) {
    MetadataCIP metadataCIP = new MetadataCIP();
    metadataCIP.setValid(false);
    Map<Object, TokenCIP> tokenMap = new HashMap<>();
    try {
      ObjectMapper objectMapper = new ObjectMapper();
      Map<Object, Object> metadataMap = objectMapper.readValue(jsonMetadata, new TypeReference<>() {
      });
      for (Map.Entry<Object, Object> metadataEntry : metadataMap.entrySet()) {
        if (metadataEntry.getValue() instanceof HashMap<?, ?> assetMap) {
          Object policyId = metadataEntry.getKey();
          for (Entry<?, ?> assetEntry : assetMap.entrySet()) {
            TokenCIP token = new TokenCIP();
            Map<Object, Object> assetValMap = (Map<Object, Object>) assetEntry.getValue();
            int musicVersion = detectMusicVersion(assetValMap.get(
                MetadataField.MUSIC_METADATA_VERSION.getName()));
            int version = MetadataCIP25Utils.detectVersion(
                metadataMap.get(MetadataField.VERSION.getName()));
            //require
            List<BaseProperty> requireProperties = new ArrayList<>();
            requireProperties.add(MetadataCIP25Utils.policy(policyId, version));
            requireProperties.add(MetadataCIP25Utils.assetName(assetEntry.getKey(), version));
            requireProperties.add(
                MetadataCIP25Utils.name(assetValMap.get(MetadataField.NAME.getName()), version));
            requireProperties.add(
                MetadataCIP25Utils.image(assetValMap.get(MetadataField.IMAGE.getName()), version));
            requireProperties.add(musicMetadataVersion(musicVersion, assetValMap.get(
                MetadataField.MUSIC_METADATA_VERSION.getName())));
            requireProperties.add(
                releaseType(assetValMap.get(MetadataField.RELEASE_TYPE.getName())));
            token.setRequireProperties(requireProperties);
            //optional
            List<BaseProperty> optionalProperties = new ArrayList<>();
            Object desc = assetValMap.get(MetadataField.DESCRIPTION.getName());
            int currentIdx = 1;
            if (Objects.nonNull(desc)) {
              optionalProperties.add(MetadataCIP25Utils.description(desc,
                  String.valueOf(currentIdx), version));
              currentIdx++;
            }
            Object mediaType = assetValMap.get(MetadataField.MEDIA_TYPE.getName());
            if (Objects.nonNull(mediaType)) {
              optionalProperties.add(
                  MetadataCIP25Utils.mediaType(mediaType, String.valueOf(currentIdx), version));
              currentIdx++;
            }
            MetadataCIP25Utils.version(metadataMap.get(MetadataField.VERSION.getName()),
                String.valueOf(currentIdx), optionalProperties);
            token.setOptionalProperties(optionalProperties);
            switch (musicVersion) {
              case 1 ->
                  setFieldsWithMusicVersionOne(assetValMap, requireProperties, optionalProperties,
                      version);
              case 2 -> setFieldsWithMusicVersionTwo(assetValMap, requireProperties, version);
              default -> {
                return metadataCIP;
              }
            }
            token.setTokenName(assetEntry.getKey());
            tokenMap.put(assetEntry.getKey(), token);
          }
        }
      }
      metadataCIP.setValid(valid(tokenMap));
    } catch (Exception ex) {
      log.error("Error: structure incorrect, message=" + ex.getMessage());
      log.error("Check standard CIP-25 fail");
    }
    metadataCIP.setTokenMap(tokenMap);
    return metadataCIP;
  }

  public static int detectMusicVersion(Object musicVersion) {
    if (Objects.isNull(musicVersion)) {
      return 0;
    } else if (musicVersion instanceof String musicVersionStr && musicVersionStr.equals("v1")) {
      return 1;
    } else if (musicVersion instanceof String musicVersionStr && musicVersionStr.equals("v2")) {
      return 2;
    } else if (musicVersion instanceof Integer musicVersionInt) {
      return musicVersionInt;
    }
    return 0;
  }

  private static void setFieldsWithMusicVersionOne(Map<Object, Object> assetValMap,
      List<BaseProperty> requireProperties, List<BaseProperty> optionalProperties, int version) {

    String releaseType = (String) assetValMap.get(MetadataField.RELEASE_TYPE.getName());
    switch (releaseType) {
      case "Single" -> {
        //require
        requireProperties.add(
            albumTitleMetadataCIP60(assetValMap.get(MetadataField.ALBUM_TITLE.getName()), "7",
                version));
        requireProperties.add(
            trackNumber(assetValMap.get(MetadataField.TRACK_NUMBER.getName()), "8",
                version));
        requireProperties.add(
            songTitle(assetValMap.get(MetadataField.SONG_TITLE.getName()), "9",
                version));
        requireProperties.add(
            songDuration(assetValMap.get(MetadataField.SONG_DURATION.getName()), "10",
                version));
        requireProperties.add(
            genres(assetValMap.get(MetadataField.GENRES.getName()), "11", version));
        requireProperties.add(
            copyright(assetValMap.get(MetadataField.COPYRIGHT.getName()), "12",
                version));
        artists(assetValMap.get(MetadataField.ARTISTS.getName()), requireProperties, "13",
            version);
        filesVerOneSingle(assetValMap.get(MetadataField.FILES.getName()), requireProperties,
            version);
        //optional
        optionalProperties.add(
            links(assetValMap.get(MetadataField.LINKS.getName()), null,
                String.valueOf(optionalProperties.size() + 1), version));
        contributingArtists(
            assetValMap.get(MetadataField.CONTRIBUTING_ARTISTS.getName()), optionalProperties,
            String.valueOf((optionalProperties.size() + 1)),
            version);
        featuredArtist(assetValMap.get(MetadataField.FEATURED_ARTIST.getName()),
            optionalProperties, String.valueOf((optionalProperties.size() + 1)), version);
        basePropertiesPartOne(assetValMap, optionalProperties, null, version);
        basePropertiesPartTwo(assetValMap, optionalProperties, null, version);
        basePropertiesPartThree(assetValMap, optionalProperties, null, version);
      }
      case "Multiple" ->
          filesVerOneMultiple(assetValMap.get(MetadataField.FILES.getName()), requireProperties,
              version);
      default -> log.warn("json metadata invalid");
    }
  }

  private static void setFieldsWithMusicVersionTwo(Map<Object, Object> assetValMap,
      List<BaseProperty> requireProperties, int version) {
    release(assetValMap.get(MetadataField.RELEASE.getName()), requireProperties,
        version);
    filesVerTwo(assetValMap.get(MetadataField.FILES.getName()), requireProperties,
        version);
  }

  private static void release(Object release, List<BaseProperty> requireProperties,
      int version) {
    BaseProperty releaseProperty = BaseProperty.builder().valid(false)
        .property(MetadataField.RELEASE.getName()).index("7").build();
    List<BaseProperty> requirePropertiesInRelease = new ArrayList<>();
    if (release instanceof HashMap<?, ?> releaseMap) {
      String ind = releaseProperty.getIndex();
      requirePropertiesInRelease.add(
          metadataString(releaseMap.get(MetadataField.RELEASE_TITLE.getName()),
              MetadataField.RELEASE_TITLE.getName(), ind, "1", version));
      requirePropertiesInRelease.add(
          metadataString(releaseMap.get(MetadataField.COPYRIGHT.getName()),
              MetadataField.COPYRIGHT.getName(), ind, "2", version));
      int index = 3;
      Object visualArtist = releaseMap.get(MetadataField.VISUAL_ARTIST.getName());
      if (Objects.nonNull(visualArtist)) {
        requirePropertiesInRelease.add(
            metadataString(visualArtist, MetadataField.VISUAL_ARTIST.getName(),
                ind, String.valueOf(index), version));
        index++;
      }
      Object distributor = releaseMap.get(MetadataField.DISTRIBUTOR.getName());
      if (Objects.nonNull(distributor)) {
        requirePropertiesInRelease.add(
            metadataString(distributor, MetadataField.DISTRIBUTOR.getName(),
                ind, String.valueOf(index), version));
        index++;
      }
      Object releaseDate = releaseMap.get(MetadataField.RELEASE_DATE.getName());
      if (Objects.nonNull(releaseDate)) {
        requirePropertiesInRelease.add(
            metadataString(releaseDate, MetadataField.RELEASE_DATE.getName(),
                ind, String.valueOf(index), version));
        index++;
      }
      Object publicationDate = releaseMap.get(MetadataField.PUBLICATION_DATE.getName());
      if (Objects.nonNull(publicationDate)) {
        requirePropertiesInRelease.add(
            metadataString(publicationDate, MetadataField.PUBLICATION_DATE.getName(),
                ind, String.valueOf(index), version));
        index++;
      }
      Object catalogNumber = releaseMap.get(MetadataField.CATALOG_NUMBER.getName());
      if (Objects.nonNull(catalogNumber)) {
        requirePropertiesInRelease.add(
            metadataInt(catalogNumber, MetadataField.CATALOG_NUMBER.getName(),
                ind, String.valueOf(index), version));
        index++;
      }
      Object releaseVersion = releaseMap.get(MetadataField.RELEASE_VERSION.getName());
      if (Objects.nonNull(releaseVersion)) {
        requirePropertiesInRelease.add(
            metadataInt(releaseVersion, MetadataField.RELEASE_VERSION.getName(),
                ind, String.valueOf(index), version));
        index++;
      }
      Object producer = releaseMap.get(MetadataField.PRODUCER.getName());
      if (Objects.nonNull(producer)) {
        requirePropertiesInRelease.add(
            metadataString(producer, MetadataField.PRODUCER.getName(), ind, String.valueOf(index),
                version));
        index++;
      }
      Object coProducer = releaseMap.get(MetadataField.CO_PRODUCER.getName());
      if (Objects.nonNull(coProducer)) {
        requirePropertiesInRelease.add(
            metadataString(coProducer, MetadataField.CO_PRODUCER.getName(),
                ind, String.valueOf(index), version));
        index++;
      }
      Object metadataLanguage = releaseMap.get(MetadataField.METADATA_LANGUAGE.getName());
      if (Objects.nonNull(metadataLanguage)) {
        requirePropertiesInRelease.add(
            metadataString(metadataLanguage, MetadataField.METADATA_LANGUAGE.getName(),
                ind, String.valueOf(index), version));
        index++;
      }
      Object language = releaseMap.get(MetadataField.LANGUAGE.getName());
      if (Objects.nonNull(language)) {
        requirePropertiesInRelease.add(
            metadataString(language, MetadataField.LANGUAGE.getName(), ind, String.valueOf(index),
                version));
        index++;
      }
      Object links = releaseMap.get(MetadataField.LINKS.getName());
      if (Objects.nonNull(links)) {
        requirePropertiesInRelease.add(links(links, ind, String.valueOf(index), version));
      }
      releaseProperty.setValid(
          requirePropertiesInRelease.stream()
              .allMatch(baseProperty -> baseProperty.getValid().equals(true)));
    }
    requireProperties.add(releaseProperty);
    if (!requirePropertiesInRelease.isEmpty()) {
      requireProperties.addAll(requirePropertiesInRelease);
    }
  }

  private static void basePropertiesPartThree(Map<?, ?> valMap,
      List<BaseProperty> baseProperties, String indexInSong, int version) {
    int index = baseProperties.size() + 1;
    Object explicit = valMap.get(MetadataField.EXPLICIT.getName());
    if (Objects.nonNull(explicit)) {
      baseProperties.add(
          metadataBool(explicit, MetadataField.EXPLICIT.getName(), indexInSong,
              String.valueOf(index),
              version));
      index++;
    }
    Object isrc = valMap.get(MetadataField.ISRC.getName());
    if (Objects.nonNull(isrc)) {
      baseProperties.add(
          metadataString(isrc, MetadataField.ISRC.getName(), indexInSong,
              String.valueOf(index),
              version));
      index++;
    }
    Object iswc = valMap.get(MetadataField.ISWC.getName());
    if (Objects.nonNull(iswc)) {
      baseProperties.add(
          metadataString(iswc, MetadataField.ISWC.getName(), indexInSong,
              String.valueOf(index),
              version));
      index++;
    }
    Object ipi = valMap.get(MetadataField.IPI.getName());
    if (Objects.nonNull(ipi)) {
      baseProperties.add(
          metadataArrayString(ipi, MetadataField.IPI.getName(), indexInSong,
              String.valueOf(index),
              version));
      index++;
    }
    Object ipn = valMap.get(MetadataField.IPN.getName());
    if (Objects.nonNull(ipn)) {
      baseProperties.add(
          metadataArrayString(ipn, MetadataField.IPN.getName(), indexInSong,
              String.valueOf(index),
              version));
      index++;
    }
    Object isni = valMap.get(MetadataField.ISNI.getName());
    if (Objects.nonNull(isni)) {
      baseProperties.add(
          metadataArrayString(isni, MetadataField.ISNI.getName(), indexInSong,
              String.valueOf(index),
              version));
      index++;
    }
    Object metadataLanguage = valMap.get(MetadataField.METADATA_LANGUAGE.getName());
    if (Objects.nonNull(metadataLanguage)) {
      baseProperties.add(
          metadataString(metadataLanguage, MetadataField.METADATA_LANGUAGE.getName(),
              indexInSong,
              String.valueOf(index), version));
      index++;
    }
    Object countryOfOrigin = valMap.get(MetadataField.COUNTRY_OF_ORIGIN.getName());
    if (Objects.nonNull(countryOfOrigin)) {
      baseProperties.add(
          metadataString(countryOfOrigin, MetadataField.COUNTRY_OF_ORIGIN.getName(),
              indexInSong,
              String.valueOf(index), version));
      index++;
    }
    Object language = valMap.get(MetadataField.LANGUAGE.getName());
    if (Objects.nonNull(language)) {
      baseProperties.add(
          metadataString(language, MetadataField.LANGUAGE.getName(),
              indexInSong,
              String.valueOf(index), version));
      index++;
    }
    Object derivedFrom = valMap.get(MetadataField.DERIVED_FROM.getName());
    if (Objects.nonNull(derivedFrom)) {
      baseProperties.add(
          metadataString(derivedFrom, MetadataField.DERIVED_FROM.getName(),
              indexInSong,
              String.valueOf(index), version));
    }
  }

  private static void basePropertiesPartTwo(Map<?, ?> valMap,
      List<BaseProperty> baseProperties, String indexInSong, int version) {
    int index = baseProperties.size() + 1;
    Object publicationDate = valMap.get(MetadataField.PUBLICATION_DATE.getName());
    if (Objects.nonNull(publicationDate)) {
      baseProperties.add(
          metadataString(publicationDate, MetadataField.PUBLICATION_DATE.getName(),
              indexInSong,
              String.valueOf(index), version));
      index++;
    }
    Object catalogNumber = valMap.get(MetadataField.CATALOG_NUMBER.getName());
    if (Objects.nonNull(catalogNumber)) {
      baseProperties.add(
          metadataInt(catalogNumber, MetadataField.CATALOG_NUMBER.getName(),
              indexInSong,
              String.valueOf(index), version));
      index++;
    }
    Object bitrate = valMap.get(MetadataField.BITRATE.getName());
    if (Objects.nonNull(bitrate)) {
      baseProperties.add(
          metadataString(bitrate, MetadataField.BITRATE.getName(), indexInSong,
              String.valueOf(index),
              version));
      index++;
    }
    Object mixEngineer = valMap.get(MetadataField.MIX_ENGINEER.getName());
    if (Objects.nonNull(mixEngineer)) {
      baseProperties.add(
          metadataString(mixEngineer, MetadataField.MIX_ENGINEER.getName(),
              indexInSong,
              String.valueOf(index), version));
      index++;
    }
    Object masteringEngineer = valMap.get(MetadataField.MASTERING_ENGINEER.getName());
    if (Objects.nonNull(masteringEngineer)) {
      baseProperties.add(
          metadataString(masteringEngineer, MetadataField.MASTERING_ENGINEER.getName(),
              indexInSong,
              String.valueOf(index), version));
      index++;
    }
    Object producer = valMap.get(MetadataField.PRODUCER.getName());
    if (Objects.nonNull(producer)) {
      baseProperties.add(
          metadataString(producer, MetadataField.PRODUCER.getName(),
              indexInSong,
              String.valueOf(index), version));
      index++;
    }
    Object coProducer = valMap.get(MetadataField.CO_PRODUCER.getName());
    if (Objects.nonNull(coProducer)) {
      baseProperties.add(
          metadataString(coProducer, MetadataField.CO_PRODUCER.getName(),
              indexInSong,
              String.valueOf(index), version));
      index++;
    }
    Object recordingEngineer = valMap.get(MetadataField.RECORDING_ENGINEER.getName());
    if (Objects.nonNull(recordingEngineer)) {
      baseProperties.add(
          metadataString(recordingEngineer, MetadataField.RECORDING_ENGINEER.getName(),
              indexInSong,
              String.valueOf(index), version));
      index++;
    }
    Object releaseVersion = valMap.get(MetadataField.RELEASE_VERSION.getName());
    if (Objects.nonNull(releaseVersion)) {
      baseProperties.add(
          metadataInt(releaseVersion, MetadataField.RELEASE_VERSION.getName(),
              indexInSong,
              String.valueOf(index), version));
      index++;
    }
    Object parentalAdvisory = valMap.get(MetadataField.PARENTAL_ADVISORY.getName());
    if (Objects.nonNull(parentalAdvisory)) {
      baseProperties.add(
          metadataString(parentalAdvisory, MetadataField.PARENTAL_ADVISORY.getName(),
              indexInSong,
              String.valueOf(index), version));
    }
  }

  private static void basePropertiesPartOne(Map<?, ?> valMap, List<BaseProperty> baseProperties,
      String indexInSong, int version) {
    Object series = valMap.get(MetadataField.SERIES.getName());
    int index = baseProperties.size() + 1;
    if (Objects.nonNull(series)) {
      baseProperties.add(
          metadataString(series, MetadataField.SERIES.getName(), indexInSong,
              String.valueOf(index),
              version));
      index++;
    }
    Object collection = valMap.get(MetadataField.COLLECTION.getName());
    if (Objects.nonNull(collection)) {
      baseProperties.add(
          metadataString(collection, MetadataField.COLLECTION.getName(),
              indexInSong,
              String.valueOf(index), version));
      index++;
    }
    Object set = valMap.get(MetadataField.SET.getName());
    if (Objects.nonNull(set)) {
      baseProperties.add(
          metadataString(set, MetadataField.SET.getName(), indexInSong,
              String.valueOf(index),
              version));
      index++;
    }
    Object mood = valMap.get(MetadataField.MOOD.getName());
    if (Objects.nonNull(mood)) {
      baseProperties.add(
          metadataString(mood, MetadataField.MOOD.getName(), indexInSong,
              String.valueOf(index),
              version));
      index++;
    }
    Object lyrics = valMap.get(MetadataField.LYRICS.getName());
    if (Objects.nonNull(lyrics)) {
      baseProperties.add(
          metadataString(lyrics, MetadataField.LYRICS.getName(), indexInSong,
              String.valueOf(index),
              version));
      index++;
    }
    Object lyricists = valMap.get(MetadataField.LYRICISTS.getName());
    if (Objects.nonNull(lyricists)) {
      baseProperties.add(
          metadataArrayString(lyricists, MetadataField.LYRICISTS.getName(),
              indexInSong,
              String.valueOf(index), version));
      index++;
    }
    Object specialThanks = valMap.get(MetadataField.SPECIAL_THANKS.getName());
    if (Objects.nonNull(specialThanks)) {
      baseProperties.add(
          metadataArrayString(specialThanks, MetadataField.SPECIAL_THANKS.getName(),
              indexInSong,
              String.valueOf(index), version));
      index++;
    }
    Object visualArtist = valMap.get(MetadataField.VISUAL_ARTIST.getName());
    if (Objects.nonNull(visualArtist)) {
      baseProperties.add(
          metadataString(visualArtist, MetadataField.VISUAL_ARTIST.getName(),
              indexInSong,
              String.valueOf(index), version));
      index++;
    }
    Object distributor = valMap.get(MetadataField.DISTRIBUTOR.getName());
    if (Objects.nonNull(distributor)) {
      baseProperties.add(
          metadataString(distributor, MetadataField.DISTRIBUTOR.getName(),
              indexInSong,
              String.valueOf(index), version));
      index++;
    }
    Object releaseDate = valMap.get(MetadataField.RELEASE_DATE.getName());
    if (Objects.nonNull(releaseDate)) {
      baseProperties.add(
          metadataString(releaseDate, MetadataField.RELEASE_DATE.getName(),
              indexInSong,
              String.valueOf(index), version));
    }
  }

  private static void filesVerOneSingle(Object files, List<BaseProperty> requireProperties,
      int version) {
    BaseProperty filesProperty = BaseProperty.builder().valid(false)
        .property(MetadataField.FILES.getName())
        .format(CommonConstant.FIELD_TYPE[8])
        .index("14")
        .build();
    List<BaseProperty> requirePropertiesInFile = new ArrayList<>();
    if (Objects.nonNull(files) && files instanceof ArrayList<?> fileList && !fileList.isEmpty()) {
      int indexInFile = 1;
      for (Object file : fileList) {
        if (file instanceof HashMap<?, ?> fileMap) {
          String index = filesProperty.getIndex() + "." + indexInFile;
          BaseProperty nameFile = MetadataCIP25Utils.nameFile(
              fileMap.get(MetadataField.NAME.getName()),
              version);
          nameFile.setIndex(index + ".1");
          requirePropertiesInFile.add(nameFile);
          BaseProperty srcFile = MetadataCIP25Utils.srcFile(
              fileMap.get(MetadataField.SRC.getName()),
              version);
          srcFile.setIndex(index + ".2");
          requirePropertiesInFile.add(srcFile);
          BaseProperty mediaTypeFile = MetadataCIP25Utils.mediaTypeFile(
              fileMap.get(MetadataField.MEDIA_TYPE.getName()), version);
          mediaTypeFile.setIndex(index + ".3");
          requirePropertiesInFile.add(mediaTypeFile);
          indexInFile++;
        }
      }
      if (version == 0) {
        filesProperty.setValid(null);
        filesProperty.setFormat(null);
      } else {
        filesProperty.setValid(requirePropertiesInFile.stream()
            .allMatch(baseProperty -> baseProperty.getValid().equals(true)));
      }
    }
    requireProperties.add(filesProperty);
    if (!requirePropertiesInFile.isEmpty()) {
      requireProperties.addAll(requirePropertiesInFile);
    }
  }

  private static void filesVerOneMultiple(Object files, List<BaseProperty> requireProperties,
      int version) {
    BaseProperty filesProperty = BaseProperty.builder().valid(false)
        .property(MetadataField.FILES.getName())
        .format(CommonConstant.FIELD_TYPE[8])
        .index("7")
        .build();
    List<BaseProperty> requirePropertiesInFile = new ArrayList<>();
    if (Objects.nonNull(files) && files instanceof ArrayList<?> fileList && !fileList.isEmpty()) {
      int indexInFile = 1;
      for (Object file : fileList) {
        if (file instanceof HashMap<?, ?> fileMap) {
          String index = filesProperty.getIndex() + "." + indexInFile;
          BaseProperty nameFile = MetadataCIP25Utils.nameFile(
              fileMap.get(MetadataField.NAME.getName()),
              version);
          nameFile.setIndex(index + ".1");
          requirePropertiesInFile.add(nameFile);
          BaseProperty srcFile = MetadataCIP25Utils.srcFile(
              fileMap.get(MetadataField.SRC.getName()),
              version);
          srcFile.setIndex(index + ".2");
          requirePropertiesInFile.add(srcFile);
          BaseProperty mediaTypeFile = MetadataCIP25Utils.mediaTypeFile(
              fileMap.get(MetadataField.MEDIA_TYPE.getName()), version);
          mediaTypeFile.setIndex(index + ".3");
          requirePropertiesInFile.add(mediaTypeFile);
          artists(fileMap.get(MetadataField.ARTISTS.getName()),
              requirePropertiesInFile, index + ".4", version);
          requirePropertiesInFile.add(
              trackNumber(fileMap.get(MetadataField.TRACK_NUMBER.getName()), index + ".5",
                  version));
          requirePropertiesInFile.add(
              songTitle(fileMap.get(MetadataField.SONG_TITLE.getName()), index + ".6", version));
          requirePropertiesInFile.add(
              songDuration(fileMap.get(MetadataField.SONG_DURATION.getName()), index + ".7",
                  version));
          requirePropertiesInFile.add(
              genres(fileMap.get(MetadataField.GENRES.getName()), index + ".8", version));
          requirePropertiesInFile.add(
              copyright(fileMap.get(MetadataField.COPYRIGHT.getName()), index + ".9", version));
          contributingArtists(
              fileMap.get(MetadataField.CONTRIBUTING_ARTISTS.getName()), requirePropertiesInFile,
              index + ".10", version);
          featuredArtist(fileMap.get(MetadataField.FEATURED_ARTIST.getName()),
              requirePropertiesInFile, index + ".11", version);
          basePropertiesPartOne(fileMap, requirePropertiesInFile, index, version);
          basePropertiesPartTwo(fileMap, requirePropertiesInFile, index, version);
          basePropertiesPartThree(fileMap, requirePropertiesInFile, index, version);
          indexInFile++;
        }
      }
      if (version == 0) {
        filesProperty.setValid(null);
        filesProperty.setFormat(null);
      } else {
        filesProperty.setValid(requirePropertiesInFile.stream()
            .allMatch(baseProperty -> baseProperty.getValid().equals(true)));
      }
    }
    requireProperties.add(filesProperty);
    if (!requirePropertiesInFile.isEmpty()) {
      requireProperties.addAll(requirePropertiesInFile);
    }
  }

  private static void filesVerTwo(Object files, List<BaseProperty> requireProperties,
      int version) {
    BaseProperty filesProperty = BaseProperty.builder().valid(false)
        .property(MetadataField.FILES.getName())
        .format(CommonConstant.FIELD_TYPE[8])
        .index("8")
        .build();
    List<BaseProperty> requirePropertiesInFile = new ArrayList<>();
    if (Objects.nonNull(files) && files instanceof ArrayList<?> fileList && !fileList.isEmpty()) {
      int indexInFile = 1;
      for (Object file : fileList) {
        if (file instanceof HashMap<?, ?> fileMap) {
          String index = filesProperty.getIndex() + "." + indexInFile;
          BaseProperty nameFile = MetadataCIP25Utils.nameFile(
              fileMap.get(MetadataField.NAME.getName()),
              version);
          nameFile.setIndex(index + ".1");
          requirePropertiesInFile.add(nameFile);
          BaseProperty srcFile = MetadataCIP25Utils.srcFile(
              fileMap.get(MetadataField.SRC.getName()),
              version);
          srcFile.setIndex(index + ".2");
          requirePropertiesInFile.add(srcFile);
          BaseProperty mediaTypeFile = MetadataCIP25Utils.mediaTypeFile(
              fileMap.get(MetadataField.MEDIA_TYPE.getName()), version);
          mediaTypeFile.setIndex(index + ".3");
          requirePropertiesInFile.add(mediaTypeFile);
          Object song = fileMap.get(MetadataField.SONG.getName());
          BaseProperty songProp = BaseProperty.builder().valid(false).index(index + ".4")
              .property(MetadataField.SONG.getName()).build();
          if (Objects.nonNull(songProp) && song instanceof HashMap<?, ?> songMap) {
            List<BaseProperty> requirePropertiesInSong = new ArrayList<>();
            String indexInSong = songProp.getIndex();
            artists(songMap.get(MetadataField.ARTISTS.getName()),
                requirePropertiesInSong, indexInSong + ".1", version);
            requirePropertiesInSong.add(
                trackNumber(songMap.get(MetadataField.TRACK_NUMBER.getName()),
                    indexInSong + ".2", version));
            requirePropertiesInSong.add(
                songTitle(songMap.get(MetadataField.SONG_TITLE.getName()),
                    indexInSong + ".3", version));
            requirePropertiesInSong.add(
                songDuration(songMap.get(MetadataField.SONG_DURATION.getName()),
                    indexInSong + ".4", version));
            requirePropertiesInSong.add(
                genres(songMap.get(MetadataField.GENRES.getName()), indexInSong + ".5",
                    version));
            requirePropertiesInSong.add(
                copyright(songMap.get(MetadataField.COPYRIGHT.getName()),
                    indexInSong + ".6", version));
            contributingArtists(
                songMap.get(MetadataField.CONTRIBUTING_ARTISTS.getName()), requirePropertiesInSong,
                indexInSong + ".7", version);
            featuredArtist(songMap.get(MetadataField.FEATURED_ARTIST.getName()),
                requirePropertiesInSong, indexInSong + ".8", version);
            basePropertiesPartOne(songMap, requirePropertiesInSong, indexInSong + ".9", version);
            basePropertiesPartTwo(songMap, requirePropertiesInSong, indexInSong + ".9", version);
            basePropertiesPartThree(songMap, requirePropertiesInSong, indexInSong + ".9", version);
            songProp.setValid(requirePropertiesInSong.stream()
                .allMatch(baseProperty -> baseProperty.getValid().equals(true)));
            requirePropertiesInFile.add(songProp);
            filesProperty.setValid(requirePropertiesInFile.stream()
                .allMatch(baseProperty -> baseProperty.getValid().equals(true)));
            requirePropertiesInFile.addAll(requirePropertiesInSong);
          }
          indexInFile++;
        }
      }
      if (version == 0) {
        filesProperty.setValid(null);
        filesProperty.setFormat(null);
      }
    }
    requireProperties.add(filesProperty);
    if (!requirePropertiesInFile.isEmpty()) {
      requireProperties.addAll(requirePropertiesInFile);
    }
  }

  private static BaseProperty links(Object links, String parentIndex, String index, int version) {
    BaseProperty baseProperty = BaseProperty.builder().value(links)
        .format(CommonConstant.FIELD_TYPE[15])
        .property(MetadataField.LINKS.getName())
        .index(Objects.isNull(parentIndex) ? index : parentIndex + "." + index)
        .valid(false)
        .build();
    if (links instanceof HashMap<?, ?> linkMap) {
      boolean valid = true;
      for (Entry<?, ?> linkEntry : linkMap.entrySet()) {
        if (!(linkEntry.getKey() instanceof String && linkEntry.getValue() instanceof String)) {
          valid = false;
          break;
        }
      }
      baseProperty.setValid(valid);
    }
    if (version == 0) {
      baseProperty.setValid(null);
      baseProperty.setFormat(null);
    }
    return baseProperty;
  }

  private static BaseProperty metadataBool(Object val, String property, String parentIndex,
      String index,
      int version) {
    BaseProperty baseProperty = BaseProperty.builder().value(val)
        .format(CommonConstant.FIELD_TYPE[14])
        .property(property)
        .index(Objects.isNull(parentIndex) ? index : parentIndex + "." + index)
        .valid(Objects.nonNull(val) && val instanceof Boolean)
        .build();
    if (version == 0) {
      baseProperty.setValid(null);
      baseProperty.setFormat(null);
    }
    return baseProperty;
  }

  private static BaseProperty metadataInt(Object val, String property, String parentIndex,
      String index,
      int version) {

    BaseProperty baseProperty = BaseProperty.builder().value(val)
        .format(CommonConstant.FIELD_TYPE[11])
        .property(property)
        .index(Objects.isNull(parentIndex) ? index : parentIndex + "." + index)
        .valid(Objects.nonNull(val) && val instanceof Integer)
        .build();
    if (version == 0) {
      baseProperty.setValid(null);
      baseProperty.setFormat(null);
    }
    return baseProperty;
  }

  private static BaseProperty metadataString(Object val, String property, String parentIndex,
      String index,
      int version) {
    BaseProperty baseProperty = BaseProperty.builder().value(val)
        .format(CommonConstant.FIELD_TYPE[0])
        .property(property)
        .index(Objects.isNull(parentIndex) ? index : parentIndex + "." + index)
        .valid(Objects.nonNull(val) && val instanceof String)
        .build();
    if (version == 0) {
      baseProperty.setValid(null);
      baseProperty.setFormat(null);
    }
    return baseProperty;
  }

  private static BaseProperty metadataArrayString(Object val, String property, String parentIndex,
      String index,
      int version) {
    BaseProperty baseProperty = BaseProperty.builder().value(val)
        .format(CommonConstant.FIELD_TYPE[13])
        .property(property)
        .index(Objects.isNull(parentIndex) ? index : parentIndex + "." + index)
        .valid(Objects.nonNull(val) && val instanceof ArrayList<?> valArr && isArrayString(valArr))
        .build();
    if (version == 0) {
      baseProperty.setValid(null);
      baseProperty.setFormat(null);
    }
    return baseProperty;
  }

  private static BaseProperty metadataArrayOrString(Object val, String property, String parentIndex,
      String index,
      int version) {
    BaseProperty baseProperty = BaseProperty.builder().value(val)
        .format(CommonConstant.FIELD_TYPE[4])
        .property(property)
        .index(Objects.isNull(parentIndex) ? index : parentIndex + "." + index)
        .valid(Objects.nonNull(val) && (val instanceof String || (val instanceof ArrayList<?> valArr
            && isArrayString(valArr))))
        .build();
    if (version == 0) {
      baseProperty.setValid(null);
      baseProperty.setFormat(null);
    }
    return baseProperty;
  }

  private static boolean isArrayString(ArrayList<?> arr) {
    return arr.stream().allMatch(String.class::isInstance);
  }

  private static void artists(Object artists, List<BaseProperty> requireProperties,
      String ind,
      int version) {
    BaseProperty artistsProperty = BaseProperty.builder().valid(false)
        .property(MetadataField.ARTISTS.getName())
        .index(ind)
        .format(CommonConstant.FIELD_TYPE[8])
        .build();
    if (Objects.nonNull(artists) && artists instanceof ArrayList<?> artistList
        && !artistList.isEmpty()) {
      List<BaseProperty> requirePropertiesInArtist = new ArrayList<>();
      int indexInArtist = 1;
      for (Object artist : artistList) {
        if (artist instanceof HashMap<?, ?> artistMap) {
          String index = ind + "." + indexInArtist;
          BaseProperty nameArtist = metadataString(
              artistMap.get(MetadataField.NAME.getName()), MetadataField.NAME.getName(),
              index, "1", version);
          requirePropertiesInArtist.add(nameArtist);
          Object image = artistMap.get(MetadataField.IMAGE.getName());
          if (Objects.nonNull(image)) {
            BaseProperty imageArtist = metadataArrayOrString(image,
                MetadataField.IMAGE.getName(), index, "2", version);
            requirePropertiesInArtist.add(imageArtist);
          }
          indexInArtist++;
        }
      }
      if (version == 0) {
        artistsProperty.setValid(null);
        artistsProperty.setFormat(null);
      } else {
        artistsProperty.setValid(
            !requirePropertiesInArtist.isEmpty() && requirePropertiesInArtist.stream()
                .allMatch(baseProperty -> baseProperty.getValid().equals(true)));
      }
      requireProperties.add(artistsProperty);
      requireProperties.addAll(requirePropertiesInArtist);
    }
  }

  private static void contributingArtists(Object contributingArtists,
      List<BaseProperty> baseProperties, String ind, int version) {
    if (Objects.nonNull(contributingArtists)
        && contributingArtists instanceof ArrayList<?> artistList
        && !artistList.isEmpty()) {
      BaseProperty artistsProperty = BaseProperty.builder().valid(false)
          .property(MetadataField.CONTRIBUTING_ARTISTS.getName())
          .format(CommonConstant.FIELD_TYPE[8])
          .index(ind)
          .build();
      List<Boolean> validArtists = new ArrayList<>();
      int indexInArtist = 1;
      for (Object artist : artistList) {
        if (artist instanceof HashMap<?, ?> artistMap) {
          String index = ind + "." + indexInArtist;
          BaseProperty nameArtist = metadataString(
              artistMap.get(MetadataField.NAME.getName()), MetadataField.NAME.getName(),
              index, "1", version);
          validArtists.add(nameArtist.getValid());
          baseProperties.add(nameArtist);
          Object image = artistMap.get(MetadataField.IMAGE.getName());
          if (Objects.nonNull(image)) {
            BaseProperty imageArtist = metadataArrayOrString(image,
                MetadataField.IMAGE.getName(), index, "2", version);
            validArtists.add(imageArtist.getValid());
            baseProperties.add(imageArtist);
          }
          indexInArtist++;
        }
      }
      if (version == 0) {
        artistsProperty.setValid(null);
        artistsProperty.setFormat(null);
      } else {
        artistsProperty.setValid(
            !validArtists.isEmpty() && validArtists.stream().allMatch(valid -> valid.equals(true)));
      }
      baseProperties.add(artistsProperty);
    }
  }

  private static void featuredArtist(Object featuredArtist,
      List<BaseProperty> baseProperties, String ind, int version) {
    if (Objects.nonNull(featuredArtist) && featuredArtist instanceof HashMap<?, ?> artistMap) {
      BaseProperty artistsProperty = BaseProperty.builder().valid(false)
          .property(MetadataField.FEATURED_ARTIST.getName())
          .format(CommonConstant.FIELD_TYPE[8])
          .index(ind)
          .build();
      List<Boolean> validArtists = new ArrayList<>();
      BaseProperty nameArtist = metadataString(
          artistMap.get(MetadataField.NAME.getName()), MetadataField.NAME.getName(),
          ind, "1", version);
      validArtists.add(nameArtist.getValid());
      baseProperties.add(nameArtist);
      Object image = artistMap.get(MetadataField.IMAGE.getName());
      if (Objects.nonNull(image)) {
        BaseProperty imageArtist = metadataArrayOrString(image,
            MetadataField.IMAGE.getName(), ind, "2", version);
        validArtists.add(imageArtist.getValid());
        baseProperties.add(imageArtist);
      }
      if (version == 0) {
        artistsProperty.setValid(null);
        artistsProperty.setFormat(null);
      } else {
        artistsProperty.setValid(
            validArtists.stream().allMatch(valid -> valid.equals(true)));
      }
      baseProperties.add(artistsProperty);
    }
  }

  private static BaseProperty copyright(Object copyright, String index, int version) {
    BaseProperty baseProperty = BaseProperty.builder().value(copyright)
        .format(CommonConstant.FIELD_TYPE[0])
        .property(MetadataField.COPYRIGHT.getName())
        .index(index)
        .valid(Objects.nonNull(copyright) && copyright instanceof String)
        .build();
    if (version == 0) {
      baseProperty.setValid(null);
      baseProperty.setFormat(null);
    }
    return baseProperty;
  }

  private static BaseProperty genres(Object genres, String index, int version) {
    BaseProperty baseProperty = BaseProperty.builder().value(genres)
        .format(CommonConstant.FIELD_TYPE[8])
        .property(MetadataField.GENRES.getName())
        .index(index)
        .valid(Objects.nonNull(genres) && genres instanceof ArrayList<?> genresArr
            && genresArr.size() <= 3 && isArrayString(genresArr))
        .build();
    if (version == 0) {
      baseProperty.setValid(null);
      baseProperty.setFormat(null);
    }
    return baseProperty;
  }

  private static BaseProperty songDuration(Object songDuration, String index,
      int version) {
    BaseProperty baseProperty = BaseProperty.builder().value(songDuration)
        .format(CommonConstant.FIELD_TYPE[12])
        .property(MetadataField.SONG_DURATION.getName())
        .index(index)
        .valid(Objects.nonNull(songDuration) && songDuration instanceof String songDurationStr
            && songDurationStr.matches(SONG_DURATION_REGEX))
        .build();
    if (version == 0) {
      baseProperty.setValid(null);
      baseProperty.setFormat(null);
    }
    return baseProperty;
  }

  private static BaseProperty songTitle(Object songTitle, String index, int version) {
    BaseProperty baseProperty = BaseProperty.builder().value(songTitle)
        .format(CommonConstant.FIELD_TYPE[4])
        .property(MetadataField.SONG_TITLE.getName())
        .index(index)
        .valid(Objects.nonNull(songTitle) && (songTitle instanceof String
            || (songTitle instanceof ArrayList<?> songTitleArr && isArrayString(songTitleArr))))
        .build();
    if (version == 0) {
      baseProperty.setValid(null);
      baseProperty.setFormat(null);
    }
    return baseProperty;
  }

  private static BaseProperty trackNumber(Object trackNumber, String index,
      int version) {
    BaseProperty baseProperty = BaseProperty.builder().value(trackNumber)
        .format(CommonConstant.FIELD_TYPE[11])
        .property(MetadataField.TRACK_NUMBER.getName())
        .index(index)
        .valid(Objects.nonNull(trackNumber) && trackNumber instanceof Integer)
        .build();
    if (version == 0) {
      baseProperty.setValid(null);
      baseProperty.setFormat(null);
    }
    return baseProperty;
  }

  private static BaseProperty albumTitleMetadataCIP60(Object albumTitle, String index,
      int version) {
    BaseProperty baseProperty = BaseProperty.builder().value(albumTitle)
        .format(CommonConstant.FIELD_TYPE[0])
        .property(MetadataField.ALBUM_TITLE.getName())
        .index(index)
        .valid(Objects.nonNull(albumTitle) && albumTitle instanceof String)
        .build();
    if (version == 0) {
      baseProperty.setValid(null);
      baseProperty.setFormat(null);
    }
    return baseProperty;
  }

  private static BaseProperty releaseType(Object releaseType) {
    return BaseProperty.builder().value(releaseType).format(CommonConstant.FIELD_TYPE[0])
        .property(MetadataField.RELEASE_TYPE.getName())
        .index("6").valid(
            Objects.nonNull(releaseType) && releaseType instanceof String releaseTypeStr && List.of(
                "Single", "Multiple").contains(releaseTypeStr)).build();
  }

  private static BaseProperty musicMetadataVersion(int musicVersion, Object originMusicVersion) {
    return BaseProperty.builder().value(originMusicVersion).format(CommonConstant.FIELD_TYPE[6])
        .property(MetadataField.MUSIC_METADATA_VERSION.getName()).index("5")
        .valid(musicVersion == 1 || musicVersion == 2)
        .build();
  }

  private static Boolean valid(Map<Object, TokenCIP> tokenMap) {
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
}
