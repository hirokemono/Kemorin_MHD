!>@file   FEM_MHD_length_scale.f90
!!@brief  module FEM_MHD_length_scale
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui on March, 2013
!
!>@brief Output FEM field data to distributed VTK file
!!
!!@verbatim
!!      subroutine allocate_work_4_lscale
!!      subroutine deallocate_work_4_lscale
!!
!!      subroutine const_MHD_length_scales                              &
!!     &         (node, iphys, nod_fld, istep_ucd, ucd)
!!      subroutine find_field_address_4_lscale(nod_fld, iphys)
!!@endverbatim
!!
!!@param istep      Step number for UCD data
!
!
      module FEM_MHD_length_scale
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_geometry_data
      use t_phys_data
!
      implicit none
!
      real(kind = kreal), allocatable :: d_mag(:)
      private :: d_mag
!
      private :: cal_vect_length_scale_by_rot
      private :: cal_length_scale_by_diffuse1
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_work_4_lscale(numnod)
!
      integer(kind = kint), intent(in) :: numnod
!
!
      allocate(d_mag(numnod))
      d_mag = zero
!
      end subroutine allocate_work_4_lscale
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_work_4_lscale
!
!
      deallocate(d_mag)
!
      end subroutine deallocate_work_4_lscale
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine const_MHD_length_scales                                &
     &         (node, iphys, nod_fld, istep_ucd, ucd)
!
      use calypso_mpi
      use t_ucd_data
      use m_phys_labels
      use m_ctl_params_4_prod_udt
      use set_and_cal_udt_data
!
      use ucd_IO_select
!
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(ucd_data), intent(inout) :: ucd
      integer(kind = kint), intent(in) :: istep_ucd
      integer(kind = kint) :: icou
!
!
      ucd%num_field = 0
      if(iphys%i_temp  .gt. 0) ucd%num_field = ucd%num_field+1
      if(iphys%i_velo  .gt. 0) ucd%num_field = ucd%num_field+1
      if(iphys%i_magne .gt. 0) ucd%num_field = ucd%num_field+1
!
      call allocate_ucd_phys_name(ucd)
!
      ucd%num_comp(1:ucd%num_field) = ione
      call cal_istack_ucd_component(ucd)
!
      ucd%nnod =      node%numnod
      call allocate_ucd_phys_data(ucd)
!
      icou = 0
      if(iphys%i_temp  .gt. 0) then
        icou = icou + 1
        ucd%phys_name(icou) = fhd_temp_scale
        call cal_length_scale_by_diffuse1                               &
     &     (iphys%i_temp, iphys%i_t_diffuse, node, nod_fld)
        call set_one_field_to_udt_data                                  &
     &     (node%numnod, ione, icou, d_mag(1), ucd)
      end if
!
      if(iphys%i_velo  .gt. 0) then
        icou = icou + 1
        ucd%phys_name(icou) = fhd_velocity_scale
        call cal_vect_length_scale_by_rot                               &
     &     (iphys%i_velo, iphys%i_vort, node, nod_fld)
        call set_one_field_to_udt_data                                  &
     &     (node%numnod, ione, icou, d_mag(1), ucd)
      end if
!
      if(iphys%i_magne .gt. 0) then
        icou = icou + 1
        ucd%phys_name(3) =    fhd_magnetic_scale
        call cal_vect_length_scale_by_rot                               &
     &     (iphys%i_magne, iphys%i_current, node, nod_fld)
        call set_one_field_to_udt_data                                  &
     &     (node%numnod, ione, icou, d_mag(1), ucd)
      end if
!
      call set_ucd_file_prefix(result_udt_file_head, ucd)
      call sel_write_udt_file(my_rank, istep_ucd, ucd)
      call deallocate_ucd_data(ucd)
!
      end subroutine const_MHD_length_scales
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine find_field_address_4_lscale(nod_fld, iphys)
!
      use m_phys_labels
!
      type(phys_data), intent(in) :: nod_fld
      type(phys_address), intent(inout) :: iphys
!
      integer(kind = kint) :: i_fld
!
!
      iphys%i_velo =      0
      iphys%i_vort =      0
      iphys%i_magne =     0
      iphys%i_current =   0
      iphys%i_temp =      0
      iphys%i_t_diffuse = 0
      do i_fld = 1, nod_fld%num_phys
        if(nod_fld%phys_name(i_fld) .eq. fhd_velo) then
          iphys%i_velo =      nod_fld%istack_component(i_fld-1) + 1
        else if(nod_fld%phys_name(i_fld) .eq. fhd_vort) then
          iphys%i_vort =      nod_fld%istack_component(i_fld-1) + 1
        else if(nod_fld%phys_name(i_fld) .eq. fhd_magne) then
          iphys%i_magne =     nod_fld%istack_component(i_fld-1) + 1
        else if(nod_fld%phys_name(i_fld) .eq. fhd_current) then
          iphys%i_current =   nod_fld%istack_component(i_fld-1) + 1
        else if(nod_fld%phys_name(i_fld) .eq. fhd_temp) then
          iphys%i_temp =      nod_fld%istack_component(i_fld-1) + 1
        else if(nod_fld%phys_name(i_fld) .eq. fhd_thermal_diffusion)   &
     &      then
          iphys%i_t_diffuse = nod_fld%istack_component(i_fld-1) + 1
        end if
      end do
!
      end subroutine find_field_address_4_lscale
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_vect_length_scale_by_rot(i_v, i_w, node, nod_fld)
!
      use mag_of_field_smp
!
      integer(kind = kint),  intent(in) :: i_v, i_w
      type(node_data), intent(in) :: node
      type(phys_data), intent(in) :: nod_fld
!
!
!$omp parallel
      call nodal_lscale_by_rot_smp                                      &
     &   (np_smp, node%numnod, node%istack_nod_smp,                     &
     &    nod_fld%ntot_phys, i_v, i_w, nod_fld%d_fld, d_mag(1))
!$omp end parallel
!
      end subroutine cal_vect_length_scale_by_rot
!
!-----------------------------------------------------------------------
!
      subroutine cal_length_scale_by_diffuse1(i_t, i_d, node, nod_fld)
!
      use mag_of_field_smp
!
      integer(kind = kint),  intent(in) :: i_t, i_d
      type(node_data), intent(in) :: node
      type(phys_data), intent(in) :: nod_fld
!
!$omp parallel
     call nodal_lscale_by_diffuse_smp                                   &
     &   (np_smp, node%numnod, node%istack_nod_smp,                     &
     &    nod_fld%ntot_phys, i_t, i_d, nod_fld%d_fld, d_mag(1))
!$omp end parallel
!
     end subroutine cal_length_scale_by_diffuse1
!
!-----------------------------------------------------------------------
!
      end module FEM_MHD_length_scale
