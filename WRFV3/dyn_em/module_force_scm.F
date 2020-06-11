module module_force_scm

! AUTHOR: Yi-Ling Hwong (CCRC)
! Forces SCM with temperature or moisture tendency 
! To implement Herman & Kuang (2013) Linear Response Function (LRF)

CONTAINS
  
  subroutine force_scm_LRF(scm_force, perturb_t, perturb_q                   & 
                           , j_pert, TtendAmp, QtendAmp                      &
                           , p, pi, thten, qvten                             &
                           , ids, ide, jds, jde, kds, kde                    &
                           , ims, ime, jms, jme, kms, kme                    &
                           , ips, ipe, jps, jpe, kps, kpe                    &
                           , its, ite, jts, jte, kts, kte                    &                                        
                           )
  
  implicit none
  
  integer, intent(in)                                     :: scm_force
  integer, intent(in)                                     :: perturb_t,perturb_q
  integer, intent(in)                                     :: j_pert ! n-th perturbation level
  real, intent(in)                                        :: TtendAmp !dT/dt perturbation (K/d)
  real, intent(in)                                        :: QtendAmp !dq/dt perturbation (kg/kg/d)
  real, dimension(ims:ime,kms:kme,jms:jme), intent(in)    :: p, pi
  real, dimension(ims:ime,kms:kme,jms:jme), intent(inout) :: thten ! potential temperature    
  real, dimension(ims:ime,kms:kme,jms:jme), intent(inout) :: qvten ! water vapor    
  integer, intent(in)                                     :: ids,ide,jds,jde,kds,kde, &
                                                             ims,ime,jms,jme,kms,kme, &  
                                                             ips,ipe,jps,jpe,kps,kpe, &
                                                             its,ite,jts,jte,kts,kte  

  ! local vars
  real, dimension(ims:ime,kms:kme,jms:jme) :: tten              ! dT/dt (K/s)
  real, dimension(ims:ime,kms:kme,jms:jme) :: qten              ! dq/dt (kg/kg/s)
  real                                     :: deltaf            ! (Kronecker?) Delta function
  integer                                  :: i,j,k
  character (len=256)                      :: message  
  
  if ( scm_force .eq. 0 ) return

  call wrf_debug ( 50, " call module_force_scm_LRF" )
    
    
  ! The forcing form is from the one used in Equation (4) of Herman and Kuang 2013
  ! The difference btw this form and the one used in Appendix A of Kuang 2012 is a factor of 1/2

  if ( perturb_t .eq. 1 ) then

      call wrf_debug ( 50, " YLH perturb_t on" )

      do i = its,ite
         do k = kts,kte
            do j = jts,jte
               
                ! Delta function
                if (k == j_pert) then
                    deltaf = 1.0
                else
                    deltaf = 0.0
                endif

                tten(i,k,j) = 0.5 * &
                         ( TtendAmp/86400 * & ! 86400 is number of s in a day
                         ( deltaf + exp(-((p(i,k,j)-p(i,j_pert,j))/7500.)**2. )))
                              

                thten(i,k,j) = thten(i,k,j) + (tten(i,k,j) / pi(i,k,j)) ! convert dT/dt to theta tendency
                !thten(i,k,j) = tten(i,k,j) ! convert dT/dt to theta tendency
                
             enddo
          enddo
      enddo

      !call wrf_debug( 50,"YLH LRF pressure")

      !do k = kts,kte
      !   
      !          ! print out value for plotting

      !          write(message, *) &
      !          k,p(1,k,1)
      !          call wrf_debug(50, message)

      !enddo   

       !call wrf_debug( 50, "YLH LRF tten")

       !do k = kts,kte

       !          ! print out value for plotting

       !          write(message, *) &
       !          k,tten(1,k,1)
       !          call wrf_debug(50, message)

       !enddo

  endif


  if ( perturb_q .eq. 1) then

      call wrf_debug ( 50, " YLH perturb_q on" )

      do i = its,ite
         do k = kts,kte
            do j = jts,jte

                ! Delta function
                if (k == j_pert) then
                    deltaf = 1.0
                else
                    deltaf = 0.0
                endif

                qten(i,k,j) = 0.5 * &
                              ( QtendAmp/86400 * & ! 86400 is number of s in a day
                              ( deltaf + exp(-((p(i,k,j)-p(i,j_pert,j))/7500.)**2. )))

                qvten(i,k,j) = qvten(i,k,j) + qten(i,k,j)

             enddo
          enddo
      enddo


      !call wrf_debug( 50,"YLH LRF pressure")

      !do k = kts,kte
      !   
      !          ! print out value for plotting

      !          write(message, *) &
      !          k,p(1,k,1)
      !          call wrf_debug(50, message)

      !enddo   

      !call wrf_debug( 50, "YLH LRF qten")

      !do k = kts,kte

      !          ! print out value for plotting

      !          write(message, *) &
      !          k,qten(1,k,1)
      !          call wrf_debug(50, message)

      !enddo

   endif

   end subroutine force_scm_LRF

end module module_force_scm
