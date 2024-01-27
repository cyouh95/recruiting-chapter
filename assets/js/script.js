$(function() {

  const modal = '<div class="modal"><span class="close">&times;</span><h3></h3><img /></div>';
  
  $('.slides section').prepend(modal);
  
  $('.slides section img').on('click', function() {
    let src = $(this).attr('src');
    
    var $slide = $(this).closest('.slide');
    $slide.find('.modal img').attr('src', src);
    
    $(this).closest('.slide').find('.modal').fadeIn();
  });
  
  $('.close').on('click', function() {
    $(this).closest('.slide').find('.modal').fadeOut();
  });
  
  Reveal.addEventListener('slidechanged', function() {
    $('.modal').fadeOut();
  });
});
