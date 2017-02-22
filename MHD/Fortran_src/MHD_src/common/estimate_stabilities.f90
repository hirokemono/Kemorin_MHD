!
!     module estimate_stabilities
!
!      Written by H. Matsui
!      Modified by H. Matsui on july, 2006
!
!!      subroutine cal_stability_4_diffuse                              &
!!     &         (ele, fl_prop, cd_prop, ht_prop, cp_prop)
!!        type(element_data), intent(in) :: ele
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in)  :: cd_prop
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!      subroutine cal_stability_4_advect                               &
!!     &         (ele, fluid, ncomp_ele, ivelo_ele, d_ele)
!
      module estimate_stabilities
!
      use m_precision
!
      use calypso_mpi
      use m_t_int_parameter
      use m_t_step_parameter
      use m_stability_data
!
      use t_physical_property
      use t_geometry_data
      use t_geometry_data_MHD
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_stability_4_diffuse                                &
     &         (ele, fl_prop, cd_prop, ht_prop, cp_prop)
!
      type(element_data), intent(in) :: ele
!
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in)  :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
!
      integer (kind = kint) :: iele
!
!
       min_length = 1.0d10
       do iele = 1, ele%numele
         min_length = min(min_length, ele%volume_ele(iele))
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
        if (fl_prop%iflag_scheme .gt. id_no_evolution) then
         cfl_diffuse = cfl_advect / fl_prop%coef_diffuse
         write(12,*) 'estimated limit for Delta t for velovity:      ', &
     &    cfl_diffuse
        end if
!
        if (ht_prop%iflag_scheme .gt. id_no_evolution) then
         cfl_diffuse = cfl_advect / ht_prop%coef_diffuse
         write(12,*) 'estimated limit for Delta t for temperature:   ', &
     &    cfl_diffuse
        end if
!
        if     (cd_prop%iflag_Bevo_scheme .gt. id_no_evolution          &
     &     .or. cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
         cfl_diffuse = cfl_advect / cd_prop%coef_diffuse
         write(12,*) 'estimated limit for Delta t for magnetic field:', &
     &    cfl_diffuse
        end if
!
        if (cp_prop%iflag_scheme .gt. id_no_evolution) then
         cfl_diffuse = cfl_advect / cp_prop%coef_diffuse
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
      subroutine cal_stability_4_advect                                 &
     &         (ele, fluid, ncomp_ele, ivelo_ele, d_ele)
!
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
!
      integer(kind = kint), intent(in) :: ncomp_ele, ivelo_ele
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
!
      integer (kind = kint) :: iele
!
!
      cfl_advect0 = 1.0d10
!
      do iele = fluid%iele_start_fld, fluid%iele_end_fld
!
        cfl_tmp = ele%volume_ele(iele)**(1/3)*2.0d0                     &
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
