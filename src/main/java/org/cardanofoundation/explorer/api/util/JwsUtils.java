package org.cardanofoundation.explorer.api.util;

import java.nio.charset.StandardCharsets;

import lombok.extern.log4j.Log4j2;

import com.nimbusds.jose.JWSAlgorithm;
import com.nimbusds.jose.JWSHeader;
import com.nimbusds.jose.JWSVerifier;
import com.nimbusds.jose.Payload;
import com.nimbusds.jose.crypto.Ed25519Verifier;
import com.nimbusds.jose.jwk.Curve;
import com.nimbusds.jose.jwk.OctetKeyPair;
import com.nimbusds.jose.util.Base64URL;
import org.apache.commons.codec.binary.Hex;
import org.erdtman.jcs.JsonCanonicalizer;

@Log4j2
public class JwsUtils {

  public static boolean verifySignatureWithEd25519(String publicKey, String signature,
                                                   String jsonData) {
    try {
      Base64URL pubKeyBase64URL = Base64URL.encode(Hex.decodeHex(publicKey));
      Base64URL sigBase64URL = Base64URL.encode(Hex.decodeHex(signature));

      JsonCanonicalizer jc = new JsonCanonicalizer(jsonData);
      Base64URL offchainBase64url = Base64URL.encode(jc.getEncodedString());

      OctetKeyPair publicJWK = new OctetKeyPair.Builder(Curve.Ed25519, pubKeyBase64URL)
          .build()
          .toPublicJWK();
      JWSHeader jwsHeader = new JWSHeader(JWSAlgorithm.EdDSA);
      JWSVerifier verifier = new Ed25519Verifier(publicJWK);
      return verifier.verify(jwsHeader,
                             composeSigningInput(jwsHeader, new Payload(offchainBase64url))
                                 .getBytes(StandardCharsets.UTF_8), sigBase64URL);
    } catch (Exception e) {
      log.error("Error while verifying signature", e);
      return false;
    }

  }

  private static String composeSigningInput(JWSHeader jwsHeader, Payload payload) {

    if (jwsHeader.isBase64URLEncodePayload()) {
      return jwsHeader.toBase64URL().toString() + '.' + payload.toBase64URL().toString();
    } else {
      return jwsHeader.toBase64URL().toString() + '.' + payload.toString();
    }
  }
}
