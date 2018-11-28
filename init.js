var initVoice = function() {
if (annyang) {
  var bigger = 1;
  Shiny.onInputChange('title', 'say title something');
  Shiny.onInputChange('color', 'black');
  Shiny.onInputChange('bigger', 1);
  Shiny.onInputChange('yes', 'no');
  Shiny.onInputChange('player1', '0');
  Shiny.onInputChange('player2', '0');
  var commands = {
    'title *title': function(title) {
      Shiny.onInputChange('title', title);
    },
    'color :color': function(color) {
      Shiny.onInputChange('color', color);
    },
    'leo :leo': function(leo) {
      Shiny.onInputChange('player1', leo);
    },
    'simon :simon': function(simon) {
      Shiny.onInputChange('player2', simon);
    },
    'bigger': function() {
      bigger += 1;
      Shiny.onInputChange('bigger', bigger);
    },
    'smaller': function() {
      if (bigger >= 1.5) {
        bigger -= 1;
        Shiny.onInputChange('bigger', bigger);
      }
    },
    'regression': function() {
      Shiny.onInputChange('yes', Math.random());
    }
  };
  annyang.addCommands(commands);
  annyang.start();
  }
};

$(function() {
  setTimeout(initVoice, 10);
});