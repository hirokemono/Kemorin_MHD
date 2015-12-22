!
!     module estimate_stabilities
!
!      Written by H. Matsui
!      Modified by H. Matsui on july, 2006
!
!      subroutine cal_stability_4_diffuse
!      subroutine cal_stability_4_advect(ncomp_ele, ivelo_ele, d_ele)
!
      module estimate_stabilities
!
      use m_precision
!
      use calypso_mpi
      use m_control_parameter
      use m_t_int_parameter
      use m_t_step_parameter
      use m_geometry_data
      use m_geometry_data_MHD
      use m_physical_property
      use m_stability_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_stability_4_diffuse
!
      integer (kind = kint) :: iele
!
!
       min_length = 1.0d10
       do iele = 1, ele1%numele
         min_length = min(min_length, ele1%volume_ele(iele))
       end do
       min_length = (min_length)**(2.0d0/3.0d0)*4.0d0 / 6.0d0
!
       if ( nprocs .gt. 1 ) then
         call MPI_allREDUCE (min_length, cfl_advect, 1,                 &
     &    CALYPSO_REAL, MPI_MIN, CALYPSO_COMM, ierr_MPI)
       else
         cfl_advect = min_length
       endif
!
      if ( my_rank .eq. 0 ) then
!
        write(12,*) ' Delta t: ', dt
        if (iflag_t_evo_4_velo .gt. id_no_evolution) then
         cfl_diffuse = cfl_advect / coef_d_velo
         write(12,*) 'estimated limit for Delta t for velovity:      ', &
     &    cfl_diffuse
        end if
!
        if (iflag_t_evo_4_temp .gt. id_no_evolution) then
         cfl_diffuse = cfl_advect / coef_d_temp
         write(12,*) 'estimated limit for Delta t for temperature:   ', &
     &    cfl_diffuse
        end if
!
        if (iflag_t_evo_4_magne .gt. id_no_evolution                    &
     &        .or. iflag_t_evo_4_vect_p .gt. id_no_evolution) then
         cfl_diffuse = cfl_advect / coef_d_magne
         write(12,*) 'estimated limit for Delta t for magnetic field:', &
     &    cfl_diffuse
        end if
!
        if (iflag_t_evo_4_composit .gt. id_no_evolution) then
         cfl_diffuse = cfl_advect / coef_d_light
         write(12,*) 'estimated limit for Delta t for composition:   ', &
     &    cfl_diffuse
        end if
!
      end if
!
       end subroutine cal_stability_4_diffuse
!
! ----------------------------------------------------------------------
!
      subroutine cal_stability_4_advect(ncomp_ele, ivelo_ele, d_ele)
!
      integer(kind = kint), intent(in) :: ncomp_ele, ivelo_ele
      real(kind = kreal), intent(in) :: d_ele(ele1%numele,ncomp_ele)
!
      integer (kind = kint) :: iele
!
!
      cfl_advect0 = 1.0d10
!
      do iele = fluid1%iele_start_fld, fluid1%iele_end_fld
!
        cfl_tmp = ele1%volume_ele(iele)**(1/3)*2.0d0                    &
     &            / (sqrt(d_ele(iele,ivelo_ele  )**2                    &
     &                  + d_ele(iele,ivelo_ele+1)**2                    &
     &                  + d_ele(iele,ivelo_ele+2)**2) + 1.0d-10)
!
        cfl_advect0 = min(cfl_advect0,cfl_tmp)
      end do
!
      call MPI_allREDUCE (cfl_advect0, cfl_advect, 1,                   &
     &  CALYPSO_REAL, MPI_MIN, CALYPSO_COMM, ierr_MPI)
!
      if ( my_rank .eq. 0 ) then
         write(12,*) 'time_step:', i_step_MHD, ' Delta t:', dt,         &
     &     'estimated CFL:', cfl_advect
      end if
!
      end subroutine cal_stability_4_advect
!
! ----------------------------------------------------------------------
!
      end module estimate_stabilities
