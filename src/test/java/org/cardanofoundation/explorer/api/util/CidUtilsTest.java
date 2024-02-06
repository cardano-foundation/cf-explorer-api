package org.cardanofoundation.explorer.api.util;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

class CidUtilsTest {

  @Test
  void verifyCid_shouldReturnTrue_whenCidIsValid() {
    String cid = "zCT5htke8Cp6yxJzyJYP1jGRzf7FCZBaxfz6rVDMrKhGsChq9ZuY";
    assertTrue(CidUtils.verifyCid(cid, preparedJsonData()));
  }

  @Test
  void verifyCid_shouldReturnFalse_whenCidIsInvalid() {
    String cid = "invalidCid";
    assertFalse(CidUtils.verifyCid(cid, preparedJsonData()));
  }

  @Test
  void verifyCid_shouldReturnFalse_whenExceptionIsThrown() {
    String cid = "zCT5htke8Cp6yxJzyJYP1jGRzf7FCZBaxfz6rVDMrKhGsChq9ZuY";
    assertFalse(CidUtils.verifyCid(cid, null));
  }

  String preparedJsonData() {
    return "{\"name\":\"Cardano Foundation\",\"ticker\":\"COT\",\"description\":\"Cardano Foundation is a non-profit organization based in Switzerland with core responsibilities to help oversee and supervise the development of Cardano, the world's first third-generation blockchain, and its ecosystem; to engage with authorities on regulatory and commercial matters and serve as a central point of contact for enterprises that wish to engage with Cardano; to work with other entities in the blockchain community to develop Cardano's global commercial ecosystem; to engage in and supervise educational campaigns designed to create general awareness of the benefits of Cardano and blockchain and their general adoption; and to nurture and support the Cardano protocol, its community, and its ecosystem.\"}";
  }
}
