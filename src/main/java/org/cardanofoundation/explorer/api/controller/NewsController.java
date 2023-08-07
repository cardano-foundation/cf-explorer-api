package org.cardanofoundation.explorer.api.controller;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.service.NewsService;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/news")
@RequiredArgsConstructor
@Tag(name = "news", description = "The news APIs")
public class NewsController {

  private final NewsService newsService;

  @GetMapping
  @LogMessage
  @Operation(
      summary = "Get news related to cardano",
      tags = {"news"})
  public ResponseEntity<Object> getNews(@RequestParam Integer limit, @RequestParam Integer offset) {
    return ResponseEntity.ok(newsService.getNews(limit, offset));
  }
}
