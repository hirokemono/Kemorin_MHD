!
!     module estimate_stabilities
!
!      Written by H. Matsui
!      Modified by H. Matsui on july, 2006
!
!      subroutine cal_stability_4_diffuse
!      subroutine cal_stability_4_advect
!
      module estimate_stabilities
!
      use m_precision
!
      use calypso_mpi
      use m_parallel_var_dof
      use m_control_parameter
      use m_t_int_parameter
      use m_t_step_parameter
      use m_geometry_parameter
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
       do iele = 1, numele
         min_length = min(min_length, volume_ele(iele))
       end do
       min_length = (min_length)**(2.0d0/3.0d0)*4.0d0 / 6.0d0
!
       if ( nprocs .gt. 1 ) then
         call MPI_allREDUCE (min_length, cfl_advect, 1,                 &
     &    MPI_DOUBLE_PRECISION, MPI_MIN, SOLVER_COMM, ierr)
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
      subroutine cal_stability_4_advect
!
      use m_element_phys_address
      use m_element_phys_data
!
      integer (kind = kint) :: iele
!
!
      cfl_advect0 = 1.0d10
!
      do iele = iele_fl_start, iele_fl_end
!
        cfl_tmp = volume_ele(iele)**(1/3)*2.0d0                         &
     &            / (sqrt(d_ele(iele,iphys_ele%i_velo  )**2             &
     &                  + d_ele(iele,iphys_ele%i_velo+1)**2             &
     &                  + d_ele(iele,iphys_ele%i_velo+2)**2) + 1.0d-10)
!
        cfl_advect0 = min(cfl_advect0,cfl_tmp)
!
      end do
!
      call MPI_allREDUCE (cfl_advect0, cfl_advect, 1,                   &
     &  MPI_DOUBLE_PRECISION, MPI_MIN, SOLVER_COMM, ierr)
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
