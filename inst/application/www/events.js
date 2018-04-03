$(function() {
  $(document).on({
    
    'shiny:busy': function(event) {
      $('#import_busy').css("visibility", "visible");
    },
    
    'shiny:idle': function(event) {
      $('#import_busy').css("visibility", "hidden");
    }
  });
  
});

function updateShinyLanguage(lg) {
  Shiny.onInputChange("language", lg);
}