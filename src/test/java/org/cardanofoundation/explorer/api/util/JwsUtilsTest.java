package org.cardanofoundation.explorer.api.util;

import org.cardanofoundation.ledgersync.common.util.JsonUtil;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class JwsUtilsTest {

  @Test
  void verifySignatureWithEd25519_shouldReturnTrue() {
    String signature = "abf3a499bd227d08554cbcf11ec6d88c82c5e5c7ad89e058d17176f3f6fadc946368111212ba7ca5de7e9bad72c907f09c99fcd6bc46b9ccb2cd43763286e703";
    String publicKey = "3b68fe80c0d71f985050906509fdc976e9209977a7fba7761ddf84bc654534f6";
    String jsonData = "{\"aging_recipient\":\"QvevriA2\",\"aging_time\":\"6 MonthsA2\",\"bottling_date\":\"2018-01-01\",\"bottling_location\":\"bottling_locationA2\",\"country_of_origin\":\"country_of_originA2\",\"fermentation_duration\":\"1 MonthA\",\"fermentation_vessel\":\"QvevriA2\",\"harvest_date\":\"2018-10-05\",\"harvest_location\":\"harvest_locationA2\",\"lot_number\":\"12340000001\",\"number_of_bottles\":2050,\"origin\":\"originA2\",\"pressing_date\":\"2018-10-05\",\"processing_location\":\"processing_locationA2\",\"produced_by\":\"ProducerA\",\"producer_address\":\"producer_addressA2\",\"producer_latitude\":21.4500004,\"producer_longitude\":24.532091,\"storage_vessel\":\"Stainless Steel\",\"varietal_name\":\"varietal_nameA2\",\"vintage_year\":2018,\"wine_color\":\"AmberA2\",\"wine_name\":\"Wine Name\",\"wine_type\":\"DryA2\"}";
    assertTrue(JwsUtils.verifySignatureWithEd25519(publicKey, signature, JsonUtil.getPrettyJson(jsonData)));
  }

  @Test
  void verifySignatureWithEd25519_shouldReturnFalse() {
    String signature = "randomsig";
    String publicKey = "3b68fe80c0d71f985050906509fdc976e9209977a7fba7761ddf84bc654534f6";
    String jsonData = "{\"aging_recipient\":\"QvevriA2\",\"aging_time\":\"6 MonthsA2\",\"bottling_date\":\"2018-01-01\",\"bottling_location\":\"bottling_locationA2\",\"country_of_origin\":\"country_of_originA2\",\"fermentation_duration\":\"1 MonthA\",\"fermentation_vessel\":\"QvevriA2\",\"harvest_date\":\"2018-10-05\",\"harvest_location\":\"harvest_locationA2\",\"lot_number\":\"12340000001\",\"number_of_bottles\":2050,\"origin\":\"originA2\",\"pressing_date\":\"2018-10-05\",\"processing_location\":\"processing_locationA2\",\"produced_by\":\"ProducerA\",\"producer_address\":\"producer_addressA2\",\"producer_latitude\":21.4500004,\"producer_longitude\":24.532091,\"storage_vessel\":\"Stainless Steel\",\"varietal_name\":\"varietal_nameA2\",\"vintage_year\":2018,\"wine_color\":\"AmberA2\",\"wine_name\":\"Wine Name\",\"wine_type\":\"DryA2\"}";
    assertFalse(JwsUtils.verifySignatureWithEd25519(publicKey, signature, JsonUtil.getPrettyJson(jsonData)));
  }

  @Test
  void verifySignatureWithEd25519_shouldReturnFalseWhenExceptionIsThrown() {
    String signature = "abf3a499bd227d08554cbcf11ec6d88c82c5e5c7ad89e058d17176f3f6fadc946368111212ba7ca5de7e9bad72c907f09c99fcd6bc46b9ccb2cd43763286e703";
    String publicKey = "3b68fe80c0d71f985050906509fdc976e9209977a7fba7761ddf84bc654534f6";
    assertFalse(JwsUtils.verifySignatureWithEd25519(publicKey, signature, null));
  }

}