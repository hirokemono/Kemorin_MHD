!
!      module m_filter_moments
!
!     Written by H. Matsui
!
!      subroutine allocate_filter_moms_nod
!      subroutine allocate_filter_moms_ele
!
!      subroutine deallocate_filter_moms_nod
!      subroutine deallocate_filter_moms_ele
!
!   data correspondence
!
!      filter_x_nod(i,ifil)...  mom_nod(ifil)%moms%f_x(i)
!      filter_y_nod(i,ifil)...  mom_nod(ifil)%moms%f_y(i)
!      filter_z_nod(i,ifil)...  mom_nod(ifil)%moms%f_z(i)
!      filter_x2_nod(i,ifil)... mom_nod(ifil)%moms%f_x2(i)
!      filter_y2_nod(i,ifil)... mom_nod(ifil)%moms%f_y2(i)
!      filter_z2_nod(i,ifil)... mom_nod(ifil)%moms%f_z2(i)
!      filter_xy_nod(i,ifil)... mom_nod(ifil)%moms%f_xy(i)
!      filter_yz_nod(i,ifil)... mom_nod(ifil)%moms%f_yz(i)
!      filter_zx_nod(i,ifil)... mom_nod(ifil)%moms%f_zx(i)
!
!      filter_x_nod_dx(i,nd,ifil)...  mom_nod(ifil)%diff%df_x(i,nd)
!      filter_y_nod_dx(i,nd,ifil)...  mom_nod(ifil)%diff%df_y(i,nd)
!      filter_z_nod_dx(i,nd,ifil)...  mom_nod(ifil)%diff%df_z(i,nd)
!      filter_x2_nod_dx(i,nd,ifil)... mom_nod(ifil)%diff%df_x2(i,nd)
!      filter_y2_nod_dx(i,nd,ifil)... mom_nod(ifil)%diff%df_y2(i,nd)
!      filter_z2_nod_dx(i,nd,ifil)... mom_nod(ifil)%diff%df_z2(i,nd)
!      filter_xy_nod_dx(i,nd,ifil)... mom_nod(ifil)%diff%df_xy(i,nd)
!      filter_yz_nod_dx(i,nd,ifil)... mom_nod(ifil)%diff%df_yz(i,nd)
!      filter_zx_nod_dx(i,nd,ifil)... mom_nod(ifil)%diff%df_zx(i,nd)
!
!
!      filter_x_ele(i,ifil)...  mom_ele(ifil)%moms%f_x(i)
!      filter_y_ele(i,ifil)...  mom_ele(ifil)%moms%f_y(i)
!      filter_z_ele(i,ifil)...  mom_ele(ifil)%moms%f_z(i)
!      filter_x2_ele(i,ifil)... mom_ele(ifil)%moms%f_x2(i)
!      filter_y2_ele(i,ifil)... mom_ele(ifil)%moms%f_y2(i)
!      filter_z2_ele(i,ifil)... mom_ele(ifil)%moms%f_z2(i)
!      filter_xy_ele(i,ifil)... mom_ele(ifil)%moms%f_xy(i)
!      filter_yz_ele(i,ifil)... mom_ele(ifil)%moms%f_yz(i)
!      filter_zx_ele(i,ifil)... mom_ele(ifil)%moms%f_zx(i)
!
!      filter_x_ele_dx(i,nd,ifil)...  mom_ele(ifil)%diff%df_x(i,nd)
!      filter_y_ele_dx(i,nd,ifil)...  mom_ele(ifil)%diff%df_y(i,nd)
!      filter_z_ele_dx(i,nd,ifil)...  mom_ele(ifil)%diff%df_z(i,nd)
!      filter_x2_ele_dx(i,nd,ifil)... mom_ele(ifil)%diff%df_x2(i,nd)
!      filter_y2_ele_dx(i,nd,ifil)... mom_ele(ifil)%diff%df_y2(i,nd)
!      filter_z2_ele_dx(i,nd,ifil)... mom_ele(ifil)%diff%df_z2(i,nd)
!      filter_xy_ele_dx(i,nd,ifil)... mom_ele(ifil)%diff%df_xy(i,nd)
!      filter_yz_ele_dx(i,nd,ifil)... mom_ele(ifil)%diff%df_yz(i,nd)
!      filter_zx_ele_dx(i,nd,ifil)... mom_ele(ifil)%diff%df_zx(i,nd)
!
!      filter_x_ele_dx2(i,nd,ifil)...  mom_ele(ifil)%diff2%df_x(i,nd)
!      filter_y_ele_dx2(i,nd,ifil)...  mom_ele(ifil)%diff2%df_y(i,nd)
!      filter_z_ele_dx2(i,nd,ifil)...  mom_ele(ifil)%diff2%df_z(i,nd)
!      filter_x2_ele_dx2(i,nd,ifil)... mom_ele(ifil)%diff2%df_x2(i,nd)
!      filter_y2_ele_dx2(i,nd,ifil)... mom_ele(ifil)%diff2%df_y2(i,nd)
!      filter_z2_ele_dx2(i,nd,ifil)... mom_ele(ifil)%diff2%df_z2(i,nd)
!      filter_xy_ele_dx2(i,nd,ifil)... mom_ele(ifil)%diff2%df_xy(i,nd)
!      filter_yz_ele_dx2(i,nd,ifil)... mom_ele(ifil)%diff2%df_yz(i,nd)
!      filter_zx_ele_dx2(i,nd,ifil)... mom_ele(ifil)%diff2%df_zx(i,nd)
!         i:     element ID
!         nd:    direction of differenciate
!         ifil:  filter ID
!
      module m_filter_moments
!
      use m_precision
      use t_filter_moments
!
      implicit none
!
      type(gradient_filter_mom_type), save :: mom1
!   mom1%mom_ele(ifil)%moms%f_x(i)
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_filter_moms_nod(nnod)
!
      integer(kind = kint), intent(in) :: nnod
!
!
      call alloc_filter_moms_nod_type(nnod, mom1)
!
      end subroutine allocate_filter_moms_nod
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_filter_moms_ele(nele)
!
      integer(kind = kint), intent(in) :: nele
!
      call alloc_filter_moms_ele_type(nele, mom1)
!
      end subroutine allocate_filter_moms_ele
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine deallocate_filter_moms_nod
!
!
      call dealloc_filter_moms_nod_type(mom1)
!
      end subroutine deallocate_filter_moms_nod
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_filter_moms_ele
!
      call dealloc_filter_moms_ele_type(mom1)
!
      end subroutine deallocate_filter_moms_ele
!
!  ---------------------------------------------------------------------
!
      end module m_filter_moments
