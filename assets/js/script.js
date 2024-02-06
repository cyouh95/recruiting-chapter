$(function() {

  const modal = '<div class="modal"><span class="close">&times;</span><img /></div>';
  
  $('.slides section').prepend(modal);
  
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
  
  Reveal.addEventListener('slidechanged', function() {
    $('.modal').fadeOut();
    $('img').removeClass('disabled');
  });
});
