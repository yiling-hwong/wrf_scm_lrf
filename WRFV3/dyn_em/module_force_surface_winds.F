module module_force_surface_winds

! AUTHOR: Yi-Ling Hwong (CCRC)
! Forces SCM surface winds 
! To implement Herman & Kuang (2013) Linear Response Function (LRF)

CONTAINS
  
  subroutine force_scm_surface_winds( force_surface_winds, u, v              &
                                     , ims, ime, jms, jme, kms, kme          &
                                     , its, ite, jts, jte, kts, kte          &                                        
                                   )
  
  implicit none
  
  integer, intent(in)                                     :: force_surface_winds
  real, dimension(ims:ime,kms:kme,jms:jme),intent(inout)  :: u,v 
  integer, intent(in)                                     :: ims,ime,jms,jme,kms,kme,   &  
                                                             its,ite,jts,jte,kts,kte  

  ! local vars
  integer                                  :: i,j
  character (len=256)                      :: message  
  
  if ( force_surface_winds .eq. 0 ) return

  call wrf_debug ( 50, " call module_force_surface_winds" )

  do i = its,ite
     do j = jts,jte
           
        u(i,kts,j) = 4.8
        v(i,kts,j) = 0.0
   
     enddo
  enddo

  !call wrf_debug( 50, "YLH U10 V10")

  !do i = its,ite

  !          ! print out value for plotting

  !          write(message, *) &
  !          i,u10(i,1),v10(i,1)
  !          call wrf_debug(50, message)

  !enddo

  end subroutine force_scm_surface_winds

end module module_force_surface_winds
