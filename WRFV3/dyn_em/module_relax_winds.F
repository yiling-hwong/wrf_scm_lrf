module module_relax_winds

! AUTHOR: Yi-Ling Hwong (CCRC)
! Relaxes U and V wind to target values 
! To implement Herman & Kuang (2013) Linear Response Function (LRF)

CONTAINS
  
  subroutine scm_relax_winds( relax_uv, tau_relax, u_target, v_target        & 
                                     , uten, vten, u, v                      &
                                     , ims, ime, jms, jme, kms, kme          &
                                     , its, ite, jts, jte, kts, kte          &                                        
                                   )
  
  implicit none
  
  integer, intent(in)                                     :: relax_uv ! switch to enable relax winds
  integer, intent(in)                                     :: tau_relax ! relaxation timescale in seconds
  real, intent(in)                                        :: u_target, v_target ! target domain mean wind speed
  real, dimension(ims:ime,kms:kme,jms:jme), intent(in)    :: u,v
  real, dimension(ims:ime,kms:kme,jms:jme), intent(inout) :: uten,vten
  integer, intent(in)                                     :: ims,ime,jms,jme,kms,kme, &  
                                                             its,ite,jts,jte,kts,kte  

  ! local vars
  integer                                  :: i,j,k
  real,dimension(ims:ime,kms:kme,jms:jme)  :: u_tend,v_tend
  character (len=256)                      :: message  
  !real, parameter                          :: u_target = 4.8
  !real, parameter                          :: v_target = 0.0
  
  if ( relax_uv .eq. 1 ) then

      call wrf_debug ( 50, " YLH relax UV" )

      !call wrf_debug( 50, "YLH u v tau_relax")

      !do k = kts,kte

      !          ! print out value for plotting

      !          write(message, *) &
      !          k,u(1,k,1),v(1,k,1),tau_relax
      !          call wrf_debug(50, message)


      !enddo

      !call wrf_debug( 50, "YLH uten vten")

      !do k = kts,kte

      !          ! print out value for plotting

      !          write(message, *) &
      !          k,uten(1,k,1),vten(1,k,1)
      !          call wrf_debug(50, message)


      !enddo      

      do i = its,ite
         do k = kts+1,kte
            do j = jts,jte
               
               u_tend(i,k,j) = -(u(i,k,j) - u_target) / tau_relax 
               v_tend(i,k,j) = -(v(i,k,j) - v_target) / tau_relax
               
               uten(i,k,j) = uten(i,k,j) + u_tend(i,k,j)
               vten(i,k,j) = vten(i,k,j) + v_tend(i,k,j)
               
            enddo  
         enddo
      enddo


      !call wrf_debug( 50, "YLH uten ven")

      !do k = kts,kte

      !          ! print out value for plotting

      !          write(message, *) &
      !          k,uten(1,k,1),vten(1,k,1)
      !          call wrf_debug(50, message)

      !enddo


  endif


  end subroutine scm_relax_winds

end module module_relax_winds
