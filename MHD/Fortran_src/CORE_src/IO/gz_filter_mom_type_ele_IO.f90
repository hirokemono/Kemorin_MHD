!gz_filter_mom_type_ele_IO.f90
!     module gz_filter_mom_type_ele_IO
!
!     Written by H. Matsui
!     modified by H. Matsui on Nov., 2006
!     modified by H. Matsui on Mar., 2008
!
!      subroutine read_base_filter_info_type_gz(filter_conf)
!      subroutine write_base_filter_info_type_gz(filter_conf)
!        type(filter_config_type), intent(inout) ::  filter_conf
!
!      subroutine read_elen_ele_type_gz(nele_fmom, elen_ele)
!      subroutine write_elen_ele_type_gz(nele_fmom, elen_ele)
!        integer (kind = kint), intent(in)  :: nele_fmom
!        type(ele_mom_diffs_type), intent(inout)  :: elen_ele
!
!      subroutine read_filter_moms_ele_type_gz(nele_fmom, mom_ele)
!      subroutine write_filter_moms_ele_type_gz(nele_fmom, mom_ele)
!        integer (kind = kint), intent(in) :: nele_fmom
!        type(ele_mom_diffs_type), intent(inout) :: mom_ele
!
      module gz_filter_mom_type_ele_IO
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_base_filter_info_type_gz(filter_conf)
!
      use t_filter_moments
      use gz_filter_moms_elen_data_IO
!
      type(filter_config_type), intent(inout) ::  filter_conf
!
!
      call read_ref_filter_param_gz(filter_conf%nf_type,                &
     &    filter_conf%filter_type,  filter_conf%f_width,                &
     &    filter_conf%xmom_1d_org)
!
      end subroutine read_base_filter_info_type_gz
!
!  ---------------------------------------------------------------------
!
      subroutine write_base_filter_info_type_gz(filter_conf)
!
      use t_filter_moments
      use gz_filter_moms_elen_data_IO
!
      type(filter_config_type), intent(in) ::  filter_conf
!
!
      call write_ref_filter_param_gz(filter_conf%nf_type,               &
     &    filter_conf%filter_type, filter_conf%f_width,                 &
     &    filter_conf%xmom_1d_org)
!
      end subroutine write_base_filter_info_type_gz
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_elen_ele_type_gz(nele_fmom, elen_ele)
!
      use t_filter_moments
      use gz_filter_moms_elen_data_IO
!
      integer (kind = kint), intent(in)  :: nele_fmom
      type(ele_mom_diffs_type), intent(inout)  :: elen_ele
!
!
      call read_elens_ele_gz(nele_fmom,                                 &
     &    elen_ele%moms%f_x2, elen_ele%moms%f_y2, elen_ele%moms%f_z2,   &
     &    elen_ele%moms%f_xy, elen_ele%moms%f_yz, elen_ele%moms%f_zx,   &
     &    elen_ele%diff%df_x2, elen_ele%diff%df_y2,                     &
     &    elen_ele%diff%df_z2, elen_ele%diff%df_xy,                     &
     &    elen_ele%diff%df_yz, elen_ele%diff%df_zx,                     &
     &    elen_ele%diff2%df_x2, elen_ele%diff2%df_y2,                   &
     &    elen_ele%diff2%df_z2, elen_ele%diff2%df_xy,                   &
     &    elen_ele%diff2%df_yz, elen_ele%diff2%df_zx)
!
      end subroutine read_elen_ele_type_gz
!
!  ---------------------------------------------------------------------
!
      subroutine write_elen_ele_type_gz(nele_fmom, elen_ele)
!
      use t_filter_moments
      use gz_filter_moms_elen_data_IO
!
!      integer (kind = kint), intent(in) :: nnod
      integer (kind = kint), intent(in)  :: nele_fmom
!      type(nod_mom_diffs_type) :: elen_nod
      type(ele_mom_diffs_type), intent(inout)  :: elen_ele
!
!
!    output coefs for filters for each node
!
      call write_elens_ele_gz(nele_fmom,                                &
     &    elen_ele%moms%f_x2, elen_ele%moms%f_y2, elen_ele%moms%f_z2,   &
     &    elen_ele%moms%f_xy, elen_ele%moms%f_yz, elen_ele%moms%f_zx,   &
     &    elen_ele%diff%df_x2, elen_ele%diff%df_y2,                     &
     &    elen_ele%diff%df_z2, elen_ele%diff%df_xy,                     &
     &    elen_ele%diff%df_yz, elen_ele%diff%df_zx,                     &
     &    elen_ele%diff2%df_x2, elen_ele%diff2%df_y2,                   &
     &    elen_ele%diff2%df_z2, elen_ele%diff2%df_xy,                   &
     &    elen_ele%diff2%df_yz, elen_ele%diff2%df_zx)
!
      end subroutine write_elen_ele_type_gz
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_filter_moms_ele_type_gz(nele_fmom, mom_ele)
!
      use t_filter_moments
      use gz_filter_moms_elen_data_IO
!
      integer (kind = kint), intent(in) :: nele_fmom
      type(ele_mom_diffs_type), intent(inout) :: mom_ele
!
!
      call read_filter_moms_ele_gz(nele_fmom,                           &
     &   mom_ele%moms%f_x2, mom_ele%moms%f_y2, mom_ele%moms%f_z2,       &
     &   mom_ele%moms%f_xy, mom_ele%moms%f_yz, mom_ele%moms%f_zx,       &
     &   mom_ele%moms%f_x, mom_ele%moms%f_y, mom_ele%moms%f_z,          &
     &   mom_ele%diff%df_x2, mom_ele%diff%df_y2, mom_ele%diff%df_z2,    &
     &   mom_ele%diff%df_xy, mom_ele%diff%df_yz, mom_ele%diff%df_zx,    &
     &   mom_ele%diff%df_x, mom_ele%diff%df_y, mom_ele%diff%df_z,       &
     &   mom_ele%diff2%df_x2, mom_ele%diff2%df_y2, mom_ele%diff2%df_z2, &
     &   mom_ele%diff2%df_xy, mom_ele%diff2%df_yz, mom_ele%diff2%df_zx, &
     &   mom_ele%diff2%df_x, mom_ele%diff2%df_y, mom_ele%diff2%df_z)
!
      end subroutine read_filter_moms_ele_type_gz
!
!  ---------------------------------------------------------------------
!
      subroutine write_filter_moms_ele_type_gz(nele_fmom, mom_ele)
!
      use t_filter_moments
      use gz_filter_moms_elen_data_IO
!
      integer (kind = kint), intent(in) :: nele_fmom
      type(ele_mom_diffs_type), intent(inout) :: mom_ele
!
!
      call write_filter_moms_ele_gz(nele_fmom,                          &
     &   mom_ele%moms%f_x2, mom_ele%moms%f_y2, mom_ele%moms%f_z2,       &
     &   mom_ele%moms%f_xy, mom_ele%moms%f_yz, mom_ele%moms%f_zx,       &
     &   mom_ele%moms%f_x, mom_ele%moms%f_y, mom_ele%moms%f_z,          &
     &   mom_ele%diff%df_x2, mom_ele%diff%df_y2, mom_ele%diff%df_z2,    &
     &   mom_ele%diff%df_xy, mom_ele%diff%df_yz, mom_ele%diff%df_zx,    &
     &   mom_ele%diff%df_x, mom_ele%diff%df_y, mom_ele%diff%df_z,       &
     &   mom_ele%diff2%df_x2, mom_ele%diff2%df_y2, mom_ele%diff2%df_z2, &
     &   mom_ele%diff2%df_xy, mom_ele%diff2%df_yz, mom_ele%diff2%df_zx, &
     &   mom_ele%diff2%df_x, mom_ele%diff2%df_y, mom_ele%diff2%df_z)
!
      end subroutine write_filter_moms_ele_type_gz
!
!  ---------------------------------------------------------------------
!
      end module gz_filter_mom_type_ele_IO
