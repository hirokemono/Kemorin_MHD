!
!      module read_udt_files_4_correlate
!
!     Written by H. Matsui on Feb., 2007
!
!!      subroutine init_udt_4_correlate                                 &
!!     &         (istep, cor_ucd_param, ref_ucd_param,                  &
!!     &          nod_fld, t_IO, ucd)
!!      subroutine read_udt_4_correlate                                 &
!!     &         (istep, cor_ucd_param, ref_ucd_param,                  &
!!     &          mgd_mesh, sec_mesh, pcor, t_IO, ucd)
!!        type(field_IO_params), intent(in) :: cor_ucd_param
!!        type(field_IO_params), intent(in) :: ref_ucd_param
!!        type(merged_mesh), intent(in) :: mgd_mesh
!!        type(second_mesh), intent(in) :: sec_mesh
!!        type(correlate_4_plane), intent(inout) :: pcor
!!        type(time_data), intent(inout) :: t_IO
!!        type(ucd_data), intent(inout) :: ucd
!
      module read_udt_files_4_correlate
!
      use m_precision
!
      use m_constants
!
      use t_time_data
      use t_ucd_data
      use t_file_IO_parameter
      use t_correlate_4_plane
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine init_udt_4_correlate                                   &
     &         (istep, cor_ucd_param, ref_ucd_param,                    &
     &          nod_fld, t_IO, ucd)
!
      use m_file_format_switch
      use t_phys_data
      use ucd_IO_select
      use copy_pick_udt_data_plane
      use cal_minmax_and_stacks
!
      integer (kind = kint), intent(in) :: istep
      type(field_IO_params), intent(in) :: cor_ucd_param
      type(field_IO_params), intent(in) :: ref_ucd_param
!
      type(phys_data), intent(inout) :: nod_fld
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd

!
!
      call init_by_ucd_4_plane_model                                    &
     &   (istep, cor_ucd_param, nod_fld, t_IO, ucd)
!
      call init_by_ucd_4_plane_model                                    &
     &   (istep, ref_ucd_param, nod_fld, t_IO, ucd)
!
      end subroutine init_udt_4_correlate
!
!-----------------------------------------------------------------------
!
      subroutine read_udt_4_correlate                                   &
     &         (istep, cor_ucd_param, ref_ucd_param,                    &
     &          mgd_mesh, sec_mesh, pcor, t_IO, ucd)
!
      use t_mesh_data_4_merge
      use m_file_format_switch
      use copy_pick_udt_data_plane
      use ucd_IO_select
!
      integer (kind = kint), intent(in) :: istep
      type(merged_mesh), intent(in) :: mgd_mesh
      type(second_mesh), intent(in) :: sec_mesh
      type(field_IO_params), intent(in) :: cor_ucd_param
      type(field_IO_params), intent(in) :: ref_ucd_param
!
      type(correlate_4_plane), intent(inout) :: pcor
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd
!
!
      call read_udt_data_4_plane_model(mgd_mesh%num_pe, istep,          &
     &    pcor%num_domain_c, pcor%num_crt,                              &
     &    pcor%icomp_crt, pcor%ifield_crt, pcor%phys_d1(1),             &
     &    mgd_mesh%merge_tbl%nnod_max, mgd_mesh%subdomain,              &
     &    cor_ucd_param, t_IO, ucd)
!
!
      call read_udt_data_4_plane_model(sec_mesh%num_pe2, istep,         &
     &    pcor%num_domain_c, pcor%num_crt,                              &
     &    pcor%icomp_crt, pcor%ifield_crt2, pcor%phys_d2(1),            &
     &    sec_mesh%merge_tbl_2%nnod_max, sec_mesh%subdomains_2,         &
     &    ref_ucd_param, t_IO, ucd)
!
      end subroutine read_udt_4_correlate
!
!-----------------------------------------------------------------------
!
      end module read_udt_files_4_correlate
