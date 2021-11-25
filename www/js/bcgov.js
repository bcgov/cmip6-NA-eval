<<<<<<< HEAD
$(document).ready(function() {

  // Back to top
	$(".back-to-top").on("click", function(e) {
		e.preventDefault();
    $('html,body').animate({ scrollTop: 0 }, 'slow');
	});
});

function findSpan(spanLabel) {
  var spans = $('.leaflet-control-layers-base').find('span');
  $.each(spans, function(index) {
    if ($(this).text().trim() == spanLabel) {
      $(this).prev().click();
    }
  });
}

=======
$(document).ready(function() {

  // Back to top
	$(".back-to-top").on("click", function(e) {
		e.preventDefault();
    $('html,body').animate({ scrollTop: 0 }, 'slow');
	});
});

function findSpan(spanLabel) {
  var spans = $('.leaflet-control-layers-base').find('span');
  $.each(spans, function(index) {
    if ($(this).text().trim() == spanLabel) {
      $(this).prev().click();
    }
  });
}

>>>>>>> 09fee0b23cc9844f67e389ca5551542427e24404
