var initVoice = function() {
if (annyang) {
  var bigger = 1;
  Shiny.onInputChange('title', 'say title something');
  Shiny.onInputChange('color', 'black');
  Shiny.onInputChange('bigger', 1);
  Shiny.onInputChange('yes', 'no');
  Shiny.onInputChange('player1', '0');
  Shiny.onInputChange('player2', '0');
  
  var simonFunction = function(simon) {
    console.log(simon);
  };
  
  var commands = {
    'title *title': function(title) {
      Shiny.onInputChange('title', title);
    },
    'color :color': function(color) {
      Shiny.onInputChange('color', color);
    },
    'leo :leo': function(leo) {
      console.log(leo);
      //Shiny.onInputChange('player1', leo);
    },
    //'simon :simon': function(simon) {
    //  Shiny.onInputChange('player2', simon);
    //},
    // /^(S|s)imon (0|1|2|3|one)$/
    //'simon :simon': {'regexp': /Simon Friday/, 'callback': function(simon) {
    //  Shiny.onInputChange('player2', simon);
    //}},
    'simon *simon': {'regexp': /^Simon[[:blank:]]\d+$/, 'callback': simonFunction}, 
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