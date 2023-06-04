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
!!     &         (node, iphys, nod_fld, istep_ucd, t_IO)
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
     &         (node, iphys, nod_fld, istep_ucd, t_IO)
!
      use calypso_mpi
      use t_time_data
      use t_ucd_data
      use m_field_product_labels
      use m_ctl_params_4_prod_udt
      use set_and_cal_udt_data
      use mag_of_field_smp
!
      use parallel_ucd_IO_select
!
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(time_data), intent(in) :: t_IO
      integer(kind = kint), intent(in) :: istep_ucd
!
      type(ucd_data) :: ucd
      integer(kind = kint) :: icou
!
!
      ucd%num_field = 0
      if(iphys%base%i_temp  .gt. 0) ucd%num_field = ucd%num_field+1
      if(iphys%base%i_velo  .gt. 0) ucd%num_field = ucd%num_field+1
      if(iphys%base%i_magne .gt. 0) ucd%num_field = ucd%num_field+1
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
      if(iphys%base%i_temp  .gt. 0) then
        icou = icou + 1
        ucd%phys_name(icou) = temperature_scale%name
!$omp parallel
        call cal_len_scale_by_diffuse_smp                               &
     &     (node%numnod, nod_fld%d_fld(1,iphys%base%i_temp),            &
     &      nod_fld%d_fld(1,iphys%diffusion%i_t_diffuse), d_mag(1))
!$omp end parallel
        call set_one_field_to_udt_data                                  &
     &     (node%numnod, ione, icou, d_mag(1), ucd)
      end if
!
      if(iphys%base%i_velo  .gt. 0) then
        icou = icou + 1
        ucd%phys_name(icou) = velocity_scale%name
!$omp parallel
        call cal_len_scale_by_rot_smp                                   &
     &     (node%numnod, nod_fld%d_fld(1,iphys%base%i_velo),            &
     &      nod_fld%d_fld(1,iphys%base%i_vort), d_mag(1))
!$omp end parallel
        call set_one_field_to_udt_data                                  &
     &     (node%numnod, ione, icou, d_mag(1), ucd)
      end if
!
      if(iphys%base%i_magne .gt. 0) then
        icou = icou + 1
        ucd%phys_name(3) =    magnetic_scale%name
!$omp parallel
        call cal_len_scale_by_rot_smp                                   &
     &     (node%numnod, nod_fld%d_fld(1,iphys%base%i_magne),           &
     &      nod_fld%d_fld(1,iphys%base%i_current), d_mag(1))
!$omp end parallel
        call set_one_field_to_udt_data                                  &
     &     (node%numnod, ione, icou, d_mag(1), ucd)
      end if
!
      call sel_write_parallel_ucd_file                                  &
     &   (istep_ucd, output_ucd_param, t_IO, ucd)
      call deallocate_ucd_data(ucd)
!
      end subroutine const_MHD_length_scales
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine find_field_address_4_lscale(nod_fld, iphys)
!
      use m_base_field_labels
      use m_diffusion_term_labels
!
      type(phys_data), intent(in) :: nod_fld
      type(phys_address), intent(inout) :: iphys
!
      integer(kind = kint) :: i_fld
!
!
      iphys%base%i_velo =      0
      iphys%base%i_vort =      0
      iphys%base%i_magne =     0
      iphys%base%i_current =   0
      iphys%base%i_temp =      0
      iphys%diffusion%i_t_diffuse = 0
      do i_fld = 1, nod_fld%num_phys
        if(nod_fld%phys_name(i_fld) .eq. velocity%name) then
          iphys%base%i_velo =      nod_fld%istack_component(i_fld-1) + 1
        else if(nod_fld%phys_name(i_fld) .eq. vorticity%name) then
          iphys%base%i_vort = nod_fld%istack_component(i_fld-1) + 1
        else if(nod_fld%phys_name(i_fld) .eq. magnetic_field%name) then
          iphys%base%i_magne = nod_fld%istack_component(i_fld-1) + 1
        else if(nod_fld%phys_name(i_fld)                                &
     &          .eq. current_density%name) then
          iphys%base%i_current = nod_fld%istack_component(i_fld-1) + 1
        else if(nod_fld%phys_name(i_fld) .eq. temperature%name) then
          iphys%base%i_temp = nod_fld%istack_component(i_fld-1) + 1
        else if(nod_fld%phys_name(i_fld) .eq. thermal_diffusion%name)   &
     &      then
          iphys%diffusion%i_t_diffuse                                   &
     &         = nod_fld%istack_component(i_fld-1) + 1
        end if
      end do
!
      end subroutine find_field_address_4_lscale
!
!-----------------------------------------------------------------------
!
      end module FEM_MHD_length_scale
