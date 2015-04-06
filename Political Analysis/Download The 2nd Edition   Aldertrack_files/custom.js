// JavaScript Document for FlatDuplex Landingpage

$(document).ready(function(){
	
	// put your address here
	var gmapAddress = '3861 Sepulveda Blvd., Culver City, CA 90230';
	
	// Navigation
	$(document).scroll(function () {
 	    var y = $(this).scrollTop();
 	    if (y > 300) {
 	        $('.navbar').fadeIn();
 	    } else {
 	        $('.navbar').fadeOut();
 	    }

 	});
	
	$('.scroll').click(function(){
		var mark = $(this).attr('id');
		var position = $('.'+mark).offset().top;
		$('html, body').animate({scrollTop:position - 90}, 'slow');
		return false;
		});
	
	// Header Slider
	$('.flexslider.notebookslider').flexslider({
		controlNav: true,
		directionNav: false
	});

	// Download Button Shake every 5 sec.
	setInterval(function(){
	    	$('#download-button').toggleClass("shake");
		}, 5000);
	
	// Info Slider
	$('.flexslider.infoslider').flexslider({
		controlNav: false,
		animation: "slide",
		slideshowSpeed: 20000,
	});
	
	// Testimonials Slider
	$('.flexslider.testimonialslider').flexslider({
		controlNav: false,
		directionNav: false
	});
	
	// Gallery Slider
	$('.flexslider.galleryslider').flexslider({
		controlNav: true,
		animation: "slide",
		slideshow: false,
		directionNav: true
	});
	
	// Gallery Lightbox
	$('.gallery-img').magnificPopup({
		delegate: 'a', // child items selector, by clicking on it popup will open
		type: 'image'
	});
	
	// Google Map
	$('.gmap').gmap3({
		 map: {
		    options: {
		      maxZoom: 16,
		      mapTypeControl: false,
		      navigationControl: false,
		      scrollwheel: false,
		      streetViewControl: false
		    }
		
		 },
		 marker:{
		    address: gmapAddress,
		 }
		},
		"autofit" );
	
	
	// Hover Actions
	$('.hover').css('opacity', '1');
	  $('.hover').hover(
	    function () {
	       $(this).stop().animate({ opacity: 0.7 }, 'slow');
	    },
	    function () {  
	       $(this).stop().animate({ opacity: 1 }, 'slow');
	  });
	  
	  // About Profile Hover
	  $('.profile').hover(
			  function(){
				  $(this).find('.profile-image').addClass('profile-image-hover');
				  $(this).find('.profile-border-arrow').addClass('profile-border-arrow-hover');
			  },
			  function(){
				  $(this).find('.profile-image').removeClass('profile-image-hover');
				  $(this).find('.profile-border-arrow').removeClass('profile-border-arrow-hover');
			  }
		
	  );
	  
	  // Pricing Table Hover
	  $('.column').hover(
			  function(){
				  $(this).addClass('column-hover');
			  },
			  function(){
				  $(this).removeClass('column-hover');
			  }
		
	  );
	  
	  // Gallery Overlay
	  $('.gallery-img a').hover(
			  function(){
				  $(this).find('.img-overlay').animate({'top': '0'}, 'fast');
			  },
			  function(){
				  $(this).find('.img-overlay').animate({'top': '100%'}, 'fast');
			  }
		
	  );
	  
	  // Scroll Top Button
	  $('.scroll-top').click(function(){
		  $("html, body").animate({ scrollTop: 0 }, 'slow');
		  return false;
	  });
	  
	  
	  // Disable form submit on enter
	  $(window).keydown(function(event){
		    if(event.keyCode == 13) {
		      event.preventDefault();
		      return false;
		    }
		  });
	  
	  
	  // Newsletter validate and send
	  var emailReg = /^[a-zA-Z0-9._-]+@([a-zA-Z0-9.-]+\.)+[a-zA-Z0-9.-]{2,4}$/;

	  // Validate
	  function validateNewsletterEmail(email,regex){
		  if (!regex.test(email.val()))
				{
					email.addClass('validation-error',500);
					$('#newsletter-form').addClass('validation-error',500);
					return false;
				}
				else
				{
					email.removeClass('validation-error',500);
					$('#newsletter-form').removeClass('validation-error',500);
					return true;
				}
	  }
	  

	  // Check and Send
	  $('#send-newsletter').click(function(){
		  // result of action
		  var result=true;

		  //Get the data from all the fields
		  var email = $('input[name=newsletter-email]');

		  // validate of name input
		  if(!validateNewsletterEmail(email,emailReg)) result=false;

		  if(result==false) return false;

		  var data = 'email=' + email.val() + '&type=newsletter';

		  //start the ajax
		  $.ajax({
			  //this is the php file that processes the data and send mail
			  url: "submit-forms.php", 
			  //POST method is used
			  type: "POST",
			  //pass the data     
			  data: data,   
			  //Do not cache the page
			  cache: false,
			  //success
			  success: function(data) {
				  $('.newsletter-form').html('<h3 class="subscribe-message">Thanks for subscribe!</h3>');
			  }
		  });

		  return false;

	  });
		
	  // Highlight Input Field
	  $('input[name=newsletter-email]').blur(function(){
		  validateNewsletterEmail($(this),emailReg); 
	  });
	  
	  
	  // Contact validate and send
	  var emailReg = /^[a-zA-Z0-9._-]+@([a-zA-Z0-9.-]+\.)+[a-zA-Z0-9.-]{2,4}$/;

	  // Validate
	  function validateEmail(email,regex){
		  if (!regex.test(email.val()))
				{
					email.addClass('validation-error',500);
					return false;
				}
				else
				{
					email.removeClass('validation-error',500);
					return true;
				}
	  }
	  
	  function validateName(name){
		  if (name.val()=='') 
          	{
            	name.addClass('validation-error',500);
            	return false;
            }
            else
            {
            	name.removeClass('validation-error',500);
            	return true;
            }
       }
	  

	  // Check and Send
	  $('#send-contact').click(function(){
		  // result of action
		  var result=true;

		  //Get the data from all the fields
		  var name = $('input[name=name]');
		  var email = $('input[name=email]');
		  var subject = $('input[name=subject]');
		  var message = $('#contact-message');

		  // validate of name input
		  if(!validateName(name)) result=false;
		  if(!validateEmail(email,emailReg)) result=false;

		  if(result==false) return false;

		  var data = 'name=' + name.val() + '&email=' + email.val() + '&subject=' + subject.val() + '&message=' + message.val() + '&type=contact';

		  //start the ajax
		  $.ajax({
			  //this is the php file that processes the data and send mail
			  url: "submit-forms.php", 
			  //POST method is used
			  type: "POST",
			  //pass the data     
			  data: data,   
			  //Do not cache the page
			  cache: false,
			  //success
			  success: function(data) {
				  $('.contact-success').fadeIn("slow");
			  }
		  });

		  return false;

	  });
		
	  // Highlight Input Field
	  $('input[name=email]').blur(function(){
		  validateEmail($(this),emailReg); 
	  });
	  
	  $('input[name=name]').blur(function(){
		  validateName($(this)); 
	  });
	  
	  

});