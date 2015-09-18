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
!!      subroutine const_MHD_length_scales(istep_ucd)
!!      subroutine find_field_address_4_lscale
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
      subroutine allocate_work_4_lscale
!
      use m_geometry_data
!
!
      allocate(d_mag(node1%numnod))
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
      subroutine const_MHD_length_scales(istep_ucd)
!
      use calypso_mpi
      use m_geometry_data
      use m_phys_labels
      use m_node_phys_address
      use m_node_phys_data
      use m_ucd_data
      use m_ctl_params_4_prod_udt
      use set_and_cal_udt_data
!
      use ucd_IO_select
!
      integer(kind = kint), intent(in) :: istep_ucd
      integer(kind = kint) :: icou
!
!
      fem_ucd%num_field = 0
      if(iphys%i_temp  .gt. 0) fem_ucd%num_field = fem_ucd%num_field+1
      if(iphys%i_velo  .gt. 0) fem_ucd%num_field = fem_ucd%num_field+1
      if(iphys%i_magne .gt. 0) fem_ucd%num_field = fem_ucd%num_field+1
!
      call allocate_ucd_phys_name(fem_ucd)
!
      fem_ucd%num_comp(1:fem_ucd%num_field) = ione
      call cal_istack_ucd_component(fem_ucd)
!
      fem_ucd%nnod =      node1%numnod
      call allocate_ucd_phys_data(fem_ucd)
!
      icou = 0
      if(iphys%i_temp  .gt. 0) then
        icou = icou + 1
        fem_ucd%phys_name(icou) = fhd_temp_scale
        call cal_length_scale_by_diffuse1(iphys%i_temp,                 &
     &      iphys%i_t_diffuse)
        call set_one_field_to_udt_data                                  &
     &     (node1%numnod, ione, icou, d_mag(1), fem_ucd)
      end if
!
      if(iphys%i_velo  .gt. 0) then
        icou = icou + 1
        fem_ucd%phys_name(icou) = fhd_velocity_scale
        call cal_vect_length_scale_by_rot(iphys%i_velo, iphys%i_vort)
        call set_one_field_to_udt_data                                  &
     &     (node1%numnod, ione, icou, d_mag(1), fem_ucd)
      end if
!
      if(iphys%i_magne .gt. 0) then
        icou = icou + 1
        fem_ucd%phys_name(3) =    fhd_magnetic_scale
        call cal_vect_length_scale_by_rot(iphys%i_magne,                &
     &      iphys%i_current)
        call set_one_field_to_udt_data                                  &
     &     (node1%numnod, ione, icou, d_mag(1), fem_ucd)
      end if
!
      call set_ucd_file_prefix(result_udt_file_head)
      call sel_write_udt_file(my_rank, istep_ucd, fem_ucd)
      call deallocate_ucd_data(fem_ucd)
!
      end subroutine const_MHD_length_scales
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine find_field_address_4_lscale
!
      use m_phys_labels
      use m_node_phys_address
      use m_node_phys_data
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
      do i_fld = 1, nod_fld1%num_phys
        if(nod_fld1%phys_name(i_fld) .eq. fhd_velo) then
          iphys%i_velo =      nod_fld1%istack_component(i_fld-1) + 1
        else if(nod_fld1%phys_name(i_fld) .eq. fhd_vort) then
          iphys%i_vort =      nod_fld1%istack_component(i_fld-1) + 1
        else if(nod_fld1%phys_name(i_fld) .eq. fhd_magne) then
          iphys%i_magne =     nod_fld1%istack_component(i_fld-1) + 1
        else if(nod_fld1%phys_name(i_fld) .eq. fhd_current) then
          iphys%i_current =   nod_fld1%istack_component(i_fld-1) + 1
        else if(nod_fld1%phys_name(i_fld) .eq. fhd_temp) then
          iphys%i_temp =      nod_fld1%istack_component(i_fld-1) + 1
        else if(nod_fld1%phys_name(i_fld) .eq. fhd_thermal_diffusion)   &
     &      then
          iphys%i_t_diffuse = nod_fld1%istack_component(i_fld-1) + 1
        end if
      end do
!
      end subroutine find_field_address_4_lscale
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_vect_length_scale_by_rot(i_v, i_w)
!
      use m_geometry_data
      use m_node_phys_data
      use mag_of_field_smp
      use mag_of_field_smp
!
      integer(kind = kint),  intent(in) :: i_v, i_w
!
!
!$omp parallel
      call nodal_lscale_by_rot_smp                                      &
     &   (np_smp, node1%numnod, node1%istack_nod_smp,                   &
     &    nod_fld1%ntot_phys, i_v, i_w, d_nod, d_mag(1))
!$omp end parallel
!
      end subroutine cal_vect_length_scale_by_rot
!
!-----------------------------------------------------------------------
!
      subroutine cal_length_scale_by_diffuse1(i_t, i_d)
!
      use m_geometry_data
      use m_node_phys_data
      use mag_of_field_smp
!
      integer(kind = kint),  intent(in) :: i_t, i_d
!
!$omp parallel
     call nodal_lscale_by_diffuse_smp                                   &
     &   (np_smp, node1%numnod, node1%istack_nod_smp,                   &
     &    nod_fld1%ntot_phys, i_t, i_d, d_nod, d_mag(1))
!$omp end parallel
!
     end subroutine cal_length_scale_by_diffuse1
!
!-----------------------------------------------------------------------
!
      end module FEM_MHD_length_scale
