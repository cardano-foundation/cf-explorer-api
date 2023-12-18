package org.cardanofoundation.explorer.api.common.enumeration;

import java.util.Arrays;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import lombok.Getter;

@Getter
public enum MetadataField {
  MUSIC_METADATA_VERSION("music_metadata_version"), RELEASE_TYPE("release_type"), ALBUM_TITLE("album_title"), TRACK_NUMBER("track_number"),
  SONG_TITLE("song_title"), SONG_DURATION("song_duration"), GENRES("genres"), COPYRIGHT("copyright"),
  ARTISTS("artists"), SERIES("series"), COLLECTION("collection"), SET("set"),
  MOOD("mood"), LYRICS("lyrics"), LYRICISTS("lyricists"), SPECIAL_THANKS("special_thanks"),
  VISUAL_ARTIST("visual_artist"), DISTRIBUTOR("distributor"), RELEASE_DATE("release_date"), PUBLICATION_DATE("publication_date"),
  CATALOG_NUMBER("catalog_number"), BITRATE("bitrate"), MIX_ENGINEER("mix_engineer"), MASTERING_ENGINEER("mastering_engineer"),
  PRODUCER("producer"), CO_PRODUCER("co_producer"), FEATURED_ARTIST("featured_artist"), RECORDING_ENGINEER("recording_engineer"),
  RELEASE_VERSION("release_version"), PARENTAL_ADVISORY("parental_advisory"), EXPLICIT("explicit"), ISRC("isrc"),
  ISWC("iswc"), IPI("ipi"), IPN("ipn"), ISNI("isni"),
  METADATA_LANGUAGE("metadata_language"), COUNTRY_OF_ORIGIN("country_of_origin"), LANGUAGE("language"), DERIVED_FROM("derived_from"),
  LINKS("links"), NAME("name"), IMAGE("image"), MEDIA_TYPE("mediaType"),
  DESCRIPTION("description"), VERSION("version"), POLICY_ID("policy_id"), ASSET_NAME("asset_name"),
  SRC("src"), FILES("files"), CONTRIBUTING_ARTISTS("contributing_artists"), RELEASE("release"), RELEASE_TITLE("release_title"),
  SONG("song")
  ;

  private final String name;
  private static final Map<String, MetadataField> fieldMap = Arrays.stream(values())
      .collect(Collectors.toMap(MetadataField::getName, Function.identity()));

  MetadataField(String name) {
    this.name = name;
  }
}
