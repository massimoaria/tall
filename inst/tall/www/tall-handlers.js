/* ============================================
   TALL - Shiny Custom Message Handlers
   ============================================ */

Shiny.addCustomMessageHandler('button_id', function(value) {
  Shiny.setInputValue('button_id', value);
});

Shiny.addCustomMessageHandler('button_id2', function(value) {
  Shiny.setInputValue('button_id2', value);
});

Shiny.addCustomMessageHandler('click', function(value) {
  Shiny.setInputValue('click', value);
});

Shiny.addCustomMessageHandler('click_dend', function(value) {
  Shiny.setInputValue('click_dend', value);
});
