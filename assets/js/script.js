$(function() {

  const modal = '<div class="modal"><span class="close">&times;</span><img /></div>';
  $('.slides section').prepend(modal);
  
  const link = '<a class="link" href="https://cyouh95.github.io/recruiting-chapter/">cyouh95.github.io/recruiting-chapter</a>';
  $('.slides section.level3').prepend(link);
  
  $('.slides section img').on('click', function() {
    let src = $(this).attr('src');
    
    var $slide = $(this).closest('.slide');
    $slide.find('.modal img').attr('src', src);
    
    $(this).closest('.slide').find('.modal').fadeIn(600);
    $(this).closest('.slide').find('p img').addClass('disabled');
  });
  
  $('.close').on('click', function() {
    $(this).closest('.slide').find('.modal').fadeOut();
    $(this).closest('.slide').find('p img').removeClass('disabled');
  });
  
  $('<hr>').insertAfter('.reveal .slide h4');
  $('<hr>').insertAfter('.reveal .slide:not(.caption) h3:not(:has(+ h4))');
  
  Reveal.addEventListener('slidechanged', function() {
    $('.modal').fadeOut();
    $('img').removeClass('disabled');
  });
  
  $('.bg').on('click', function(e) {
    $('.backgrounds').attr('class', 'backgrounds ' + $(this).attr('id'))
  })
});
