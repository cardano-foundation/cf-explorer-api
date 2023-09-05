package org.cardanofoundation.explorer.api.service.impl;

import java.io.FileNotFoundException;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.cardanofoundation.explorer.api.service.GenesisFetching;
import org.cardanofoundation.explorer.api.util.FileUtil;
import org.springframework.stereotype.Service;
import org.springframework.util.ResourceUtils;

@Service
@RequiredArgsConstructor
@Log4j2
public class GenesisFetchingImpl implements GenesisFetching {

  @Override
  public String getContent(String url) {
    try {
      return FileUtil.readFile(ResourceUtils.getFile(url).getAbsolutePath());
    } catch (FileNotFoundException e) {
      log.error("exception {} with url {}", e.getMessage(), url);
      throw new IllegalStateException("can't load file " + url);
    }
  }
}
