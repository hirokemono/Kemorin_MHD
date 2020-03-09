!>@file   check_MHD_dependency_by_id.f90
!!@brief  module check_MHD_dependency_by_id
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2007
!
!>@brief  Check dependecy of field list fro MHD dynamo
!!
!!@verbatim
!!      subroutine check_dependencies_by_id(cd_prop, iphys, fld)
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(in) :: fld
!!@endverbatim
!
      module check_MHD_dependency_by_id
!
      use m_precision
      use m_error_IDs
      use m_phys_labels
!
      use calypso_mpi
!
      use t_physical_property
      use t_phys_address
      use t_phys_data
!
      implicit none
!
      private :: check_missing_field
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine check_dependencies_by_id(cd_prop, iphys, fld)
!
      type(conductive_property), intent(in) :: cd_prop
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: fld
!
      integer(kind = kint) :: i, i_start
!
!
      do i = 1, fld%num_phys
        i_start = fld%istack_component(i-1) + 1
        if(     i_start .eq. iphys%i_filter_velo                        &
     &     .or. i_start .eq. iphys%i_vort                               &
     &     .or. i_start .eq. iphys%i_press                              &
     &     .or. i_start .eq. iphys%i_magne                              &
     &     .or. i_start .eq. iphys%i_temp                               &
     &     .or. i_start .eq. iphys%i_light                              &
     &     .or. i_start .eq. iphys%i_v_diffuse                          &
     &     .or. i_start .eq. iphys%i_square_v) then
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_velo, fhd_velo)
        else if(i_start .eq. iphys%i_filter_vort                        &
     &     .or. i_start .eq. iphys%i_velo_scale                         &
     &     .or. i_start .eq. iphys%i_k_heli                             &
     &     .or. i_start .eq. iphys%i_square_w) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_vort, fhd_vort)
!
        else if(i_start .eq. iphys%i_filter_magne                       &
     &     .or. i_start .eq. iphys%i_current                            &
     &     .or. i_start .eq. iphys%i_b_diffuse                          &
     &     .or. i_start .eq. iphys%i_mag_p                              &
     &     .or. i_start .eq. iphys%i_square_b) then
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_magne, fhd_magne)
        else if(i_start .eq. iphys%i_filter_vecp                        &
     &     .or. i_start .eq. iphys%i_scalar_p                           &
     &     .or. i_start .eq. iphys%i_m_heli                             &
     &     .or. i_start .eq. iphys%i_vp_diffuse                         &
     &     .or. i_start .eq. iphys%i_square_a) then
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_vecp, fhd_vecp)
        else if(i_start .eq. iphys%i_filter_current                     &
     &     .or. i_start .eq. iphys%i_c_heli                             &
     &     .or. i_start .eq. iphys%i_magne_scale                        &
     &     .or. i_start .eq. iphys%i_square_j) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_current, fhd_current)
        else if(i_start .eq. iphys%i_vecp) then
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_velo, fhd_velo)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_magne, fhd_magne)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_mag_p, fhd_mag_potential)
!
        else if(i_start .eq. iphys%i_t_diffuse                          &
     &     .or. i_start .eq. iphys%i_per_temp                           &
     &     .or. i_start .eq. iphys%i_filter_temp                        &
     &     .or. i_start .eq. iphys%i_heat_source                        &
     &     .or. i_start .eq. iphys%i_square_t) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_temp, fhd_temp)
        else if(i_start .eq. iphys%i_filter_comp                        &
     &     .or. i_start .eq. iphys%i_c_diffuse                          &
     &     .or. i_start .eq. iphys%i_square_c                           &
     &     .or. i_start .eq. iphys%i_light_source) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_light, fhd_light)
        else if(i_start .eq. iphys%i_entropy_source) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_entropy, fhd_entropy)
        end if
      end do
!
      do i = 1, fld%num_phys
        i_start = fld%istack_component(i-1) + 1
        if(i_start .eq. iphys%i_temp_scale) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_t_diffuse, fhd_thermal_diffusion)
        else if(i_start .eq. iphys%i_comp_scale) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_c_diffuse, fhd_c_diffuse)
        else if(i_start .eq. iphys%forces%i_mag_stretch) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_magne, fhd_magne)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%diff_vector%i_grad_vx, grad_v_1%name)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%diff_vector%i_grad_vy, grad_v_2%name)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%diff_vector%i_grad_vz, grad_v_3%name)
!
        else if(i_start .eq. iphys%i_per_entropy) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_entropy, fhd_per_entropy)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_ref_entropy, fhd_ref_entropy)
!
        else if(i_start .eq. iphys%i_x_heli) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_velo, fhd_velo)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_magne, fhd_magne)
        else if(i_start .eq. iphys%i_entropy) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_velo, fhd_velo)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_temp, fhd_temp)
        end if
      end do
!
      do i = 1, fld%num_phys
        i_start = fld%istack_component(i-1) + 1
        if(i_start .eq. iphys%i_density) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_velo, fhd_velo)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_temp, fhd_temp)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_light, fhd_light)
        else if(i_start .eq. iphys%i_per_density) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_density, fhd_density)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_ref_density, fhd_ref_density)
        else if(i_start .eq. iphys%i_poynting) then 
          call check_missing_field                                      &
     &       (fld, i_start, iphys%forces%i_vp_induct, vecp_induction%name)
          call check_missing_field                                      &
     &       (fld, i_start, iphys%i_current, fhd_current)
        end if
      end do
!
      end subroutine check_dependencies_by_id
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_missing_field(fld, iphys_tgt, iphys_ref, name)
!
      type(phys_data), intent(in) :: fld
      integer(kind = kint), intent(in) :: iphys_tgt, iphys_ref
      character(len = kchara), intent(in) :: name
!
      if(iphys_ref .gt. 0) return
      write(*,*) 'Following fields are required for ',                  &
     &     trim(field_name_by_address(fld, iphys_tgt)),                 &
     &     ': ', trim(name)
      call calypso_MPI_abort(ierr_fld,'Stop program.')
!
      end subroutine check_missing_field
!
! -----------------------------------------------------------------------
!
     end module check_MHD_dependency_by_id
