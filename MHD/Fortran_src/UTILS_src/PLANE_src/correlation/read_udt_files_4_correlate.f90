!
!      module read_udt_files_4_correlate
!
!     Written by H. Matsui on Feb., 2007
!
!      subroutine init_udt_4_correlate(istep, nod_fld, t_IO, ucd)
!      subroutine read_udt_4_correlate(istep, t_IO, ucd)
!
      module read_udt_files_4_correlate
!
      use m_precision
!
      use m_constants
      use m_correlate_4_plane
!
      use t_time_data_IO
      use t_ucd_data
!
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine init_udt_4_correlate(istep, nod_fld, t_IO, ucd)
!
      use m_geometry_data_4_merge
      use m_file_format_switch
      use t_phys_data
      use ucd_IO_select
      use copy_pick_udt_data_plane
      use cal_minmax_and_stacks
!
      integer (kind = kint), intent(in) :: istep
      type(phys_data), intent(inout) :: nod_fld
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd

!
!
      ucd%ifmt_file = itype_cor_ucd_file
      ucd%file_prefix = cor_udt_header
      call init_by_ucd_4_plane_model(istep, nod_fld, t_IO, ucd)
!
      ucd%ifmt_file = itype_ref_ucd_file
      ucd%file_prefix = ref_udt_header
      call init_by_ucd_4_plane_model(istep, nod_fld, t_IO, ucd)
!
      end subroutine init_udt_4_correlate
!
!-----------------------------------------------------------------------
!
      subroutine read_udt_4_correlate(istep, t_IO, ucd)
!
      use m_geometry_data_4_merge
      use m_2nd_geometry_4_merge
      use m_file_format_switch
      use copy_pick_udt_data_plane
      use ucd_IO_select
!
      integer (kind = kint), intent(in) :: istep
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd
!
!
      ucd%ifmt_file = itype_cor_ucd_file
      ucd%file_prefix = cor_udt_header
      call read_udt_data_4_plane_model(num_pe, istep,                   &
     &    num_domain, num_crt, icomp_crt, ifield_crt, phys_d1(1),       &
     &    merge_tbl%nnod_max, subdomain, t_IO, ucd)
!
!
      ucd%ifmt_file = itype_ref_ucd_file
      ucd%file_prefix = ref_udt_header
      call read_udt_data_4_plane_model(num_pe2, istep,                  &
     &    num_domain, num_crt, icomp_crt, ifield_crt2, phys_d2(1),      &
     &    merge_tbl_2%nnod_max, subdomains_2, t_IO, ucd)
!
      end subroutine read_udt_4_correlate
!
!-----------------------------------------------------------------------
!
      end module read_udt_files_4_correlate
