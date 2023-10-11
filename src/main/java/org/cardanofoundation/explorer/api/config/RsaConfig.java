package org.cardanofoundation.explorer.api.config;

import jakarta.annotation.PostConstruct;
import java.security.PublicKey;
import lombok.Getter;
import lombok.Setter;
import org.cardanofoundation.explorer.common.utils.RsaUtils;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

@Component
@ConfigurationProperties("rsa.key")
@Getter
@Setter
public class RsaConfig {

  private String publicKeyStr;

  private PublicKey publicKey;

  @PostConstruct
  public void createRsaKey() {
    java.security.Security.addProvider(new org.bouncycastle.jce.provider.BouncyCastleProvider());
    publicKey = RsaUtils.getPublicKey(publicKeyStr);
  }
}
