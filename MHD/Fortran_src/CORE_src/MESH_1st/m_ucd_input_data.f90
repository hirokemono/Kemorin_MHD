!>@file   m_ucd_input_data.f90
!!@brief  module m_ucd_input_data
!!
!!@author H. Matsui
!!@date Programmed in July, 2013
!
!>@brief Arrays for Field data IO
!!
!!@verbatim
!!      subroutine set_control_input_ucd_file_def
!!      subroutine set_input_ucd_file_prefix(file_prefix)
!!      subroutine init_read_ucd_data(my_rank, istep_ucd)
!!      subroutine allocate_phys_data_by_output(my_rank, istep_ucd)
!!      subroutine set_data_by_read_ucd(my_rank, istep_ucd)
!!
!!      subroutine set_data_by_read_ucd_once(my_rank, istep_ucd,        &
!!     &          ifile_format, ucd_prefix)
!!      subroutine add_ucd_to_data(my_rank, istep_ucd,                  &
!!     &          ifile_format, ucd_prefix)
!!      subroutine subtract_by_ucd_data(my_rank, istep_ucd,             &
!!     &          ifile_format, ucd_prefix)
!!
!!      subroutine find_field_id_in_read_ucd(my_rank, istep_ucd,        &
!!     &          ifile_format, ucd_prefix, field_name,                 &
!!     &          i_field, ncomp_field)
!!      subroutine set_one_field_by_read_ucd_once(my_rank, istep_ucd,   &
!!     &          ifile_format, ucd_prefix, i_field, ncomp_field,       &
!!     &          numnod, d_fld)
!!@endverbatim
!
      module m_ucd_input_data
!
      use m_precision
      use m_field_file_format
      use m_file_format_switch
!
      use t_ucd_data
!
      implicit none
!
!>        Instance for FEM field data IO
      type(ucd_data), save :: input_ucd
!
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_control_input_ucd_file_def
!
      use ucd_IO_select
!
      call set_ucd_file_define(input_ucd)
!
      end subroutine set_control_input_ucd_file_def
!
! -----------------------------------------------------------------------
!
      subroutine set_input_ucd_file_prefix(file_prefix)
!
      character(len=kchara), intent(in) :: file_prefix
!
      input_ucd%file_prefix = file_prefix
!
      end subroutine set_input_ucd_file_prefix
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine init_read_ucd_data(my_rank, istep_ucd)
!
      use m_geometry_data
      use m_node_phys_data
!
      use ucd_IO_select
!
      integer(kind = kint),  intent(in) :: my_rank, istep_ucd
!
!
      input_ucd%nnod =      node1%numnod
      input_ucd%ntot_comp = nod_fld1%num_phys_viz
      call sel_read_udt_param(my_rank, istep_ucd, input_ucd)
!
      end subroutine init_read_ucd_data
!
! -----------------------------------------------------------------------
!
      subroutine allocate_phys_data_by_output(my_rank, istep_ucd)
!
      use m_node_phys_data
      use cal_minmax_and_stacks
      use ucd_IO_select
!
      integer(kind = kint),  intent(in) :: my_rank, istep_ucd
!
!
      call init_read_ucd_data(my_rank, istep_ucd)
      nod_fld1%num_phys = input_ucd%num_field
!
      call allocate_phys_name
!
      nod_fld1%num_component(1:nod_fld1%num_phys)                       &
     &     = input_ucd%num_comp(1:nod_fld1%num_phys)
      phys_nod_name(1:nod_fld1%num_phys)                                &
     &     = input_ucd%phys_name(1:nod_fld1%num_phys)
!
      call s_cal_total_and_stacks                                       &
     &   (nod_fld1%num_phys, nod_fld1%num_component,                    &
     &    izero, istack_nod_component, nod_fld1%ntot_phys)
      nod_fld1%ntot_phys_viz = nod_fld1%ntot_phys
!
      call allocate_data_arrays
!
      end subroutine allocate_phys_data_by_output
!
!-----------------------------------------------------------------------
!
      subroutine set_data_by_read_ucd(my_rank, istep_ucd)
!
      use m_geometry_data
      use m_node_phys_data
!
      use set_and_cal_udt_data
      use ucd_IO_select
!
      integer(kind = kint),  intent(in) :: my_rank, istep_ucd
!
!
      call sel_read_udt_file(my_rank, istep_ucd, input_ucd)
      call set_field_by_udt_data(node1%numnod, nod_fld1%num_phys,       &
     &    nod_fld1%ntot_phys, istack_nod_component, phys_nod_name,      &
     &    d_nod, input_ucd)
!
      end subroutine set_data_by_read_ucd
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_data_by_read_ucd_once(my_rank, istep_ucd,          &
     &          ifile_format, ucd_prefix)
!
      use m_geometry_data
      use m_node_phys_data
!
      use set_and_cal_udt_data
      use ucd_IO_select
!
      character(len=kchara), intent(in) :: ucd_prefix
      integer(kind = kint),  intent(in) :: ifile_format
      integer(kind = kint),  intent(in) :: my_rank, istep_ucd
!
      type(ucd_data) :: local_ucd
!
!
      local_ucd%ifmt_file =   ifile_format
      local_ucd%file_prefix = ucd_prefix
!
      local_ucd%nnod =      node1%numnod
      call sel_read_alloc_udt_file(my_rank, istep_ucd, local_ucd)
      call set_field_by_udt_data(node1%numnod, nod_fld1%num_phys,       &
     &    nod_fld1%ntot_phys, istack_nod_component, phys_nod_name,      &
     &    d_nod, local_ucd)
      call deallocate_ucd_data(local_ucd)
!
      end subroutine set_data_by_read_ucd_once
!
! -----------------------------------------------------------------------
!
      subroutine add_ucd_to_data(my_rank, istep_ucd,                    &
     &          ifile_format, ucd_prefix)
!
      use m_geometry_data
      use m_node_phys_data
!
      use set_and_cal_udt_data
      use ucd_IO_select
!
      character(len=kchara), intent(in) :: ucd_prefix
      integer(kind = kint),  intent(in) :: ifile_format
      integer(kind = kint),  intent(in) :: my_rank, istep_ucd
!
      type(ucd_data) :: local_ucd
!
!
      local_ucd%ifmt_file =   ifile_format
      local_ucd%file_prefix = ucd_prefix
!
      local_ucd%nnod =      node1%numnod
      call sel_read_alloc_udt_file(my_rank, istep_ucd, local_ucd)
      call add_field_by_udt_data(node1%numnod, nod_fld1%num_phys,       &
     &    nod_fld1%ntot_phys, istack_nod_component, phys_nod_name,      &
     &    d_nod, local_ucd)
      call deallocate_ucd_data(local_ucd)
!
      end subroutine add_ucd_to_data
!
! -----------------------------------------------------------------------
!
      subroutine subtract_by_ucd_data(my_rank, istep_ucd,               &
     &          ifile_format, ucd_prefix)
!
      use m_geometry_data
      use m_node_phys_data
!
      use set_and_cal_udt_data
      use ucd_IO_select
!
      character(len=kchara), intent(in) :: ucd_prefix
      integer(kind = kint),  intent(in) :: ifile_format
      integer(kind = kint),  intent(in) :: my_rank, istep_ucd
!
      type(ucd_data) :: local_ucd
!
!
      local_ucd%ifmt_file =   ifile_format
      local_ucd%file_prefix = ucd_prefix
!
      local_ucd%nnod = node1%numnod
      call sel_read_alloc_udt_file(my_rank, istep_ucd, local_ucd)
      call subtract_field_by_udt_data(node1%numnod, nod_fld1%num_phys,  &
     &    nod_fld1%ntot_phys, istack_nod_component, phys_nod_name,      &
     &    d_nod, local_ucd)
      call deallocate_ucd_data(local_ucd)
!
      end subroutine subtract_by_ucd_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine find_field_id_in_read_ucd(my_rank, istep_ucd,          &
     &          ifile_format, ucd_prefix, field_name,                   &
     &          i_field, ncomp_field)
!
      use m_geometry_data
      use m_node_phys_data
!
      use set_and_cal_udt_data
      use ucd_IO_select
!
      character(len=kchara), intent(in) :: ucd_prefix
      integer(kind = kint),  intent(in) :: ifile_format
      integer(kind = kint),  intent(in) :: my_rank, istep_ucd
!
      character(len = kchara), intent(in) :: field_name
      integer(kind = kint),  intent(inout) :: i_field, ncomp_field
!
      type(ucd_data) :: local_ucd
!
!
      local_ucd%ifmt_file =   ifile_format
      local_ucd%file_prefix = ucd_prefix
!
      local_ucd%nnod =      node1%numnod
      call sel_read_udt_param(my_rank, istep_ucd, local_ucd)
      call find_field_id_in_ucd(local_ucd, field_name,                  &
     &    i_field, ncomp_field)
      call deallocate_ucd_data(local_ucd)
!
      end subroutine find_field_id_in_read_ucd
!
! -----------------------------------------------------------------------
!
      subroutine set_one_field_by_read_ucd_once(my_rank, istep_ucd,     &
     &          ifile_format, ucd_prefix, i_field, ncomp_field,         &
     &          numnod, d_fld)
!
      use set_and_cal_udt_data
      use ucd_IO_select
!
      character(len=kchara), intent(in) :: ucd_prefix
      integer(kind = kint),  intent(in) :: ifile_format
      integer(kind = kint),  intent(in) :: my_rank, istep_ucd
      integer(kind = kint),  intent(in) :: numnod, i_field, ncomp_field
      real(kind = kreal), intent(inout) :: d_fld(numnod,ncomp_field)
!
      type(ucd_data) :: local_ucd
!
!
      local_ucd%ifmt_file =   ifile_format
      local_ucd%file_prefix = ucd_prefix
!
      local_ucd%nnod =      numnod
      call sel_read_alloc_udt_file(my_rank, istep_ucd, local_ucd)
      call set_one_field_by_udt_data(numnod, ncomp_field,               &
     &    i_field, d_fld, local_ucd)
      call deallocate_ucd_data(local_ucd)
!
      end subroutine set_one_field_by_read_ucd_once
!
! -----------------------------------------------------------------------
!
      end module m_ucd_input_data
