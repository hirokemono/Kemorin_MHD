!filter_moments_on_ele_IO_b.f90
!     module filter_moments_on_ele_IO_b
!
!     Written by H. Matsui
!     modified by H. Matsui on Nov., 2006
!     modified by H. Matsui on Mar., 2008
!
!      subroutine read_base_filter_info_b(id_file)
!      subroutine write_base_filter_info_b(id_file)
!
!      subroutine read_elength_ele_b(id_file)
!      subroutine write_elength_ele_b(id_file)
!      subroutine read_filter_moments_ele_b(id_file, ifil)
!      subroutine write_filter_moments_ele_b(id_file, ifil)
!
      module filter_moments_on_ele_IO_b
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
      subroutine read_base_filter_info_b(id_file)
!
      use m_filter_elength
      use filter_moms_elen_data_IO_b
!
      integer(kind = kint), intent(in) :: id_file
!
!
      call read_ref_filter_param_b                                      &
     &   (id_file, FEM1_elen%filter_conf%nf_type,                       &
     &    FEM1_elen%filter_conf%filter_type,                            &
     &    FEM1_elen%filter_conf%f_width,                                &
     &    FEM1_elen%filter_conf%xmom_1d_org)
!
      end subroutine read_base_filter_info_b
!
!  ---------------------------------------------------------------------
!
      subroutine write_base_filter_info_b(id_file)
!
      use m_filter_elength
      use filter_moms_elen_data_IO_b
!
      integer(kind = kint), intent(in) :: id_file
!
!
      call write_ref_filter_param_b                                     &
     &   (id_file, FEM1_elen%filter_conf%nf_type,                       &
     &    FEM1_elen%filter_conf%filter_type,                            &
     &    FEM1_elen%filter_conf%f_width,                                &
     &    FEM1_elen%filter_conf%xmom_1d_org)
!
      end subroutine write_base_filter_info_b
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_elength_ele_b(id_file)
!
      use m_filter_elength
      use filter_moms_elen_data_IO_b
!
      integer(kind = kint), intent(in) :: id_file
!
!
      call read_elens_ele_b(id_file, FEM1_elen%nele_filter_mom,         &
     &  FEM1_elen%elen_ele%moms%f_x2,   FEM1_elen%elen_ele%moms%f_y2,   &
     &  FEM1_elen%elen_ele%moms%f_z2,   FEM1_elen%elen_ele%moms%f_xy,   &
     &  FEM1_elen%elen_ele%moms%f_yz,   FEM1_elen%elen_ele%moms%f_zx,   &
     &  FEM1_elen%elen_ele%diff%df_x2,  FEM1_elen%elen_ele%diff%df_y2,  &
     &  FEM1_elen%elen_ele%diff%df_z2,  FEM1_elen%elen_ele%diff%df_xy,  &
     &  FEM1_elen%elen_ele%diff%df_yz,  FEM1_elen%elen_ele%diff%df_zx,  &
     &  FEM1_elen%elen_ele%diff2%df_x2, FEM1_elen%elen_ele%diff2%df_y2, &
     &  FEM1_elen%elen_ele%diff2%df_z2, FEM1_elen%elen_ele%diff2%df_xy, &
     &  FEM1_elen%elen_ele%diff2%df_yz, FEM1_elen%elen_ele%diff2%df_zx)
!
      end subroutine read_elength_ele_b
!
!  ---------------------------------------------------------------------
!
      subroutine write_elength_ele_b(id_file)
!
      use m_filter_elength
      use filter_moms_elen_data_IO_b
!
      integer(kind = kint), intent(in) :: id_file
!
!
      call write_elens_ele_b(id_file, FEM1_elen%nele_filter_mom,        &
     &  FEM1_elen%elen_ele%moms%f_x2,   FEM1_elen%elen_ele%moms%f_y2,   &
     &  FEM1_elen%elen_ele%moms%f_z2,   FEM1_elen%elen_ele%moms%f_xy,   &
     &  FEM1_elen%elen_ele%moms%f_yz,   FEM1_elen%elen_ele%moms%f_zx,   &
     &  FEM1_elen%elen_ele%diff%df_x2,  FEM1_elen%elen_ele%diff%df_y2,  &
     &  FEM1_elen%elen_ele%diff%df_z2,  FEM1_elen%elen_ele%diff%df_xy,  &
     &  FEM1_elen%elen_ele%diff%df_yz,  FEM1_elen%elen_ele%diff%df_zx,  &
     &  FEM1_elen%elen_ele%diff2%df_x2, FEM1_elen%elen_ele%diff2%df_y2, &
     &  FEM1_elen%elen_ele%diff2%df_z2, FEM1_elen%elen_ele%diff2%df_xy, &
     &  FEM1_elen%elen_ele%diff2%df_yz, FEM1_elen%elen_ele%diff2%df_zx)
!
      end subroutine write_elength_ele_b
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_filter_moments_ele_b(id_file, ifil)
!
      use m_filter_moments
      use filter_moms_elen_data_IO_b
!
      integer(kind = kint), intent(in) :: id_file, ifil
!
!
      call read_filter_moms_ele_b(id_file, nele_fmom,                   &
     &    filter_x2_ele(1,ifil), filter_y2_ele(1,ifil),                 &
     &    filter_z2_ele(1,ifil), filter_xy_ele(1,ifil),                 &
     &    filter_yz_ele(1,ifil), filter_zx_ele(1,ifil),                 &
     &    filter_x_ele(1,ifil), filter_y_ele(1,ifil),                   &
     &    filter_z_ele(1,ifil), filter_x2_ele_dx(1,1,ifil),             &
     &    filter_y2_ele_dx(1,1,ifil), filter_z2_ele_dx(1,1,ifil),       &
     &    filter_xy_ele_dx(1,1,ifil), filter_yz_ele_dx(1,1,ifil),       &
     &    filter_zx_ele_dx(1,1,ifil), filter_x_ele_dx(1,1,ifil),        &
     &    filter_y_ele_dx(1,1,ifil),  filter_z_ele_dx(1,1,ifil),        &
     &    filter_x2_ele_dx2(1,1,ifil), filter_y2_ele_dx2(1,1,ifil),     &
     &    filter_z2_ele_dx2(1,1,ifil), filter_xy_ele_dx2(1,1,ifil),     &
     &    filter_yz_ele_dx2(1,1,ifil), filter_zx_ele_dx2(1,1,ifil),     &
     &    filter_x_ele_dx2(1,1,ifil), filter_y_ele_dx2(1,1,ifil),       &
     &    filter_z_ele_dx2(1,1,ifil) )
!
      end subroutine read_filter_moments_ele_b
!
!  ---------------------------------------------------------------------
!
      subroutine write_filter_moments_ele_b(id_file, ifil)
!
      use m_filter_moments
      use filter_moms_elen_data_IO_b
!
      integer(kind = kint), intent(in) :: id_file, ifil
!
!
      call write_filter_moms_ele_b(id_file, nele_fmom,                  &
     &    filter_x2_ele(1,ifil), filter_y2_ele(1,ifil),                 &
     &    filter_z2_ele(1,ifil), filter_xy_ele(1,ifil),                 &
     &    filter_yz_ele(1,ifil), filter_zx_ele(1,ifil),                 &
     &    filter_x_ele(1,ifil), filter_y_ele(1,ifil),                   &
     &    filter_z_ele(1,ifil), filter_x2_ele_dx(1,1,ifil),             &
     &    filter_y2_ele_dx(1,1,ifil), filter_z2_ele_dx(1,1,ifil),       &
     &    filter_xy_ele_dx(1,1,ifil), filter_yz_ele_dx(1,1,ifil),       &
     &    filter_zx_ele_dx(1,1,ifil), filter_x_ele_dx(1,1,ifil),        &
     &    filter_y_ele_dx(1,1,ifil),  filter_z_ele_dx(1,1,ifil),        &
     &    filter_x2_ele_dx2(1,1,ifil), filter_y2_ele_dx2(1,1,ifil),     &
     &    filter_z2_ele_dx2(1,1,ifil), filter_xy_ele_dx2(1,1,ifil),     &
     &    filter_yz_ele_dx2(1,1,ifil), filter_zx_ele_dx2(1,1,ifil),     &
     &    filter_x_ele_dx2(1,1,ifil), filter_y_ele_dx2(1,1,ifil),       &
     &    filter_z_ele_dx2(1,1,ifil) )
!
      end subroutine write_filter_moments_ele_b
!
!  ---------------------------------------------------------------------
!
      end module filter_moments_on_ele_IO_b
