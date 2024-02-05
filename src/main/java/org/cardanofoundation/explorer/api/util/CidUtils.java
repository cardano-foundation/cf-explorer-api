package org.cardanofoundation.explorer.api.util;

import java.io.IOException;

import lombok.extern.log4j.Log4j2;

import com.bloxbean.cardano.client.crypto.Blake2bUtil;
import io.ipfs.cid.Cid;
import io.ipfs.multibase.Multibase;
import io.ipfs.multihash.Multihash;
import org.erdtman.jcs.JsonCanonicalizer;

@Log4j2
public class CidUtils {

  public static boolean verifyCid(String cid, String jsonData) {
    try {
      String json = new JsonCanonicalizer(jsonData).getEncodedString();
      return cid.equals(generateCid(json));
    } catch (Exception e) {
      log.error("Error while verifying cid", e);
      return false;
    }
  }

  private static String generateCid(String canonicalized) {
    // Get bytes to hash.
    byte[] canonicalizedBytes = canonicalized.getBytes();

    // Blake2b-256 as the hashing algorithm.
    byte[] hashed = Blake2bUtil.blake2bHash256(canonicalizedBytes);

    // Content Identifier (CID) as defined by Multiformats (Protocol Labs).
    Cid cid = new Cid(1, Cid.Codec.Raw, Multihash.Type.blake2b_256, hashed);

    // CID be represented in Multibase format with base58btc encoding.
    return Multibase.encode(Multibase.Base.Base58BTC, cid.toBytes());
  }
}
