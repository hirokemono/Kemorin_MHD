!>@file   int_element_length.f90
!!@brief  module int_element_length
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in Aug., 2006
!
!>@brief  obtain length of element
!!
!!@verbatim
!!      subroutine s_int_element_length(nele_filter, node, ele,         &
!!   &          g_FEM, spf_3d, dxi_ele, elen_ele)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(volume_shape_function), intent(in) :: spf_3d
!!        type(dxdxi_direction_type), intent(inout) :: dxi_ele
!!        type(elen_on_ele_type), intent(inout) :: elen_ele
!!@endverbatim
!
      module int_element_length
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_geometry_constants
!
      implicit none
!
      private :: fem_element_length, cal_element_length_by_jacobi
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_int_element_length(nele_filter, node, ele,           &
     &          g_FEM, spf_3d, dxi_ele, elen_ele)
!
      use t_geometry_data
      use t_filter_dxdxi
      use t_filter_elength
      use t_fem_gauss_int_coefs
      use t_shape_functions
!
!
      integer(kind = kint), intent(in) :: nele_filter
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(volume_shape_function), intent(in) :: spf_3d
      type(dxdxi_direction_type), intent(inout) :: dxi_ele
      type(elen_on_ele_type), intent(inout) :: elen_ele
!
!
        call fem_element_length                                         &
     &     (node%numnod, ele%numele, ele%nnod_4_ele, node%xx, ele%ie,   &
     &      ele%istack_ele_smp, g_FEM%max_int_point, nele_filter,       &
     &      g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3, &
     &      g_FEM%owe3d, g_FEM%maxtot_int_3d,                           &
     &      spf_3d%dnxi, spf_3d%dnei, spf_3d%dnzi,                      &
     &      dxi_ele%dx%df_dxi, dxi_ele%dx%df_dei, dxi_ele%dx%df_dzi,    &
     &      dxi_ele%dy%df_dxi, dxi_ele%dy%df_dei, dxi_ele%dy%df_dzi,    &
     &      dxi_ele%dz%df_dxi, dxi_ele%dz%df_dei, dxi_ele%dz%df_dzi)
!
      call cal_element_length_by_jacobi                                 &
     &     (ele%istack_ele_smp, nele_filter,                            &
     &      dxi_ele%dx%df_dxi, dxi_ele%dx%df_dei, dxi_ele%dx%df_dzi,    &
     &      dxi_ele%dy%df_dxi, dxi_ele%dy%df_dei, dxi_ele%dy%df_dzi,    &
     &      dxi_ele%dz%df_dxi, dxi_ele%dz%df_dei, dxi_ele%dz%df_dzi,    &
     &      elen_ele%f_x2,  elen_ele%f_y2,  elen_ele%f_z2,              &
     &      elen_ele%f_xy,  elen_ele%f_yz,  elen_ele%f_zx)
!
      end subroutine s_int_element_length
!
!-----------------------------------------------------------------------
!
      subroutine cal_element_length_by_jacobi                           &
     &         (iele_smp_stack, nele_filter,                            &
     &          dxdxi_ele, dxdei_ele, dxdzi_ele,                        &
     &          dydxi_ele, dydei_ele, dydzi_ele,                        &
     &          dzdxi_ele, dzdei_ele, dzdzi_ele,                        &
     &          elen_dx2_ele,  elen_dy2_ele,  elen_dz2_ele,             &
     &          elen_dxdy_ele, elen_dydz_ele, elen_dzdx_ele)
!
      integer(kind = kint), intent(in) :: iele_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: nele_filter
!
      real(kind=kreal), intent(inout) :: dxdxi_ele(nele_filter)
      real(kind=kreal), intent(inout) :: dxdei_ele(nele_filter)
      real(kind=kreal), intent(inout) :: dxdzi_ele(nele_filter)
      real(kind=kreal), intent(inout) :: dydxi_ele(nele_filter)
      real(kind=kreal), intent(inout) :: dydei_ele(nele_filter)
      real(kind=kreal), intent(inout) :: dydzi_ele(nele_filter)
      real(kind=kreal), intent(inout) :: dzdxi_ele(nele_filter)
      real(kind=kreal), intent(inout) :: dzdei_ele(nele_filter)
      real(kind=kreal), intent(inout) :: dzdzi_ele(nele_filter)
!
      real(kind=kreal), intent(inout) :: elen_dx2_ele(nele_filter)
      real(kind=kreal), intent(inout) :: elen_dy2_ele(nele_filter)
      real(kind=kreal), intent(inout) :: elen_dz2_ele(nele_filter)
      real(kind=kreal), intent(inout) :: elen_dxdy_ele(nele_filter)
      real(kind=kreal), intent(inout) :: elen_dydz_ele(nele_filter)
      real(kind=kreal), intent(inout) :: elen_dzdx_ele(nele_filter)
!
      integer(kind = kint) :: iproc, iele
      integer(kind = kint) :: ist, ied
!
!
!$omp parallel do private(iele,ist,ied)
      do iproc = 1, np_smp
        ist = iele_smp_stack(iproc-1)+1
        ied = iele_smp_stack(iproc)
        do iele = ist, ied
          dxdxi_ele(iele) = dxdxi_ele(iele) * r125
          dxdei_ele(iele) = dxdei_ele(iele) * r125
          dxdzi_ele(iele) = dxdzi_ele(iele) * r125
!
          dydxi_ele(iele) = dydxi_ele(iele) * r125
          dydei_ele(iele) = dydei_ele(iele) * r125
          dydzi_ele(iele) = dydzi_ele(iele) * r125
!
          dzdxi_ele(iele) = dzdxi_ele(iele) * r125
          dzdei_ele(iele) = dzdei_ele(iele) * r125
          dzdzi_ele(iele) = dzdzi_ele(iele) * r125
!
          elen_dx2_ele(iele) = ( dxdxi_ele(iele)*dxdxi_ele(iele)        &
     &                         + dxdei_ele(iele)*dxdei_ele(iele)        &
     &                         + dxdzi_ele(iele)*dxdzi_ele(iele) )
          elen_dy2_ele(iele) = ( dydxi_ele(iele)*dydxi_ele(iele)        &
     &                         + dydei_ele(iele)*dydei_ele(iele)        &
     &                         + dydzi_ele(iele)*dydzi_ele(iele) )
          elen_dz2_ele(iele) = ( dzdxi_ele(iele)*dzdxi_ele(iele)        &
     &                         + dzdei_ele(iele)*dzdei_ele(iele)        &
     &                         + dzdzi_ele(iele)*dzdzi_ele(iele) )
!
          elen_dxdy_ele(iele) = ( dxdxi_ele(iele)*dydxi_ele(iele)       &
     &                          + dxdei_ele(iele)*dydei_ele(iele)       &
     &                          + dxdzi_ele(iele)*dydzi_ele(iele) )
          elen_dydz_ele(iele) = ( dydxi_ele(iele)*dzdxi_ele(iele)       &
     &                          + dydei_ele(iele)*dzdei_ele(iele)       &
     &                          + dydzi_ele(iele)*dzdzi_ele(iele) )
          elen_dzdx_ele(iele) = ( dzdxi_ele(iele)*dxdxi_ele(iele)       &
     &                          + dzdei_ele(iele)*dxdei_ele(iele)       &
     &                          + dzdzi_ele(iele)*dxdzi_ele(iele) )
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_element_length_by_jacobi
!
!-----------------------------------------------------------------------
!
      subroutine fem_element_length(numnod, numele, nnod_4_e,           &
     &          xx, ie, iele_smp_stack, n_int, nele_filter,             &
     &          max_int_point, maxtot_int_3d, int_start3, owe3d,        &
     &          maxtot_int, dnxi, dnei, dnzi,                           &
     &          dxdxi_ele, dxdei_ele, dxdzi_ele,                        &
     &          dydxi_ele, dydei_ele, dydzi_ele,                        &
     &          dzdxi_ele, dzdei_ele, dzdzi_ele)
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_e
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_e)
      integer(kind = kint), intent(in) :: iele_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in) :: max_int_point, maxtot_int_3d
      integer(kind = kint), intent(in) :: int_start3(max_int_point)
      real(kind = kreal),   intent(in) :: owe3d(maxtot_int_3d)
!
      integer(kind = kint), intent(in) :: maxtot_int
      real(kind = kreal), intent(in) :: dnxi(nnod_4_e,maxtot_int)
      real(kind = kreal), intent(in) :: dnei(nnod_4_e,maxtot_int)
      real(kind = kreal), intent(in) :: dnzi(nnod_4_e,maxtot_int)
!
      integer(kind = kint), intent(in) :: n_int
      integer(kind = kint), intent(in) :: nele_filter
      real(kind=kreal), intent(inout) :: dxdxi_ele(nele_filter)
      real(kind=kreal), intent(inout) :: dxdei_ele(nele_filter)
      real(kind=kreal), intent(inout) :: dxdzi_ele(nele_filter)
      real(kind=kreal), intent(inout) :: dydxi_ele(nele_filter)
      real(kind=kreal), intent(inout) :: dydei_ele(nele_filter)
      real(kind=kreal), intent(inout) :: dydzi_ele(nele_filter)
      real(kind=kreal), intent(inout) :: dzdxi_ele(nele_filter)
      real(kind=kreal), intent(inout) :: dzdei_ele(nele_filter)
      real(kind=kreal), intent(inout) :: dzdzi_ele(nele_filter)
!
      integer(kind = kint) :: iproc, iele
      integer(kind = kint) :: ii, ix, ist, ied
      integer(kind = kint) :: inod(nnod_4_e)
      real(kind = kreal) :: xe(nnod_4_e), dxi(nnod_4_e)
!
!
!$omp parallel do private(iele,ii,ix,ist,ied,inod,xe,dxi)
      do iproc = 1, np_smp
        ist = iele_smp_stack(iproc-1)+1
        ied = iele_smp_stack(iproc)
!
        dxdxi_ele(ist:ied) = 0.0d0
        dxdei_ele(ist:ied) = 0.0d0
        dxdzi_ele(ist:ied) = 0.0d0
        dydxi_ele(ist:ied) = 0.0d0
        dydei_ele(ist:ied) = 0.0d0
        dydzi_ele(ist:ied) = 0.0d0
        dzdxi_ele(ist:ied) = 0.0d0
        dzdei_ele(ist:ied) = 0.0d0
        dzdzi_ele(ist:ied) = 0.0d0
!
        do ii = 1, n_int * n_int * n_int
          ix = int_start3(n_int) + ii
!
          do iele = ist, ied
            inod(1:nnod_4_e) = ie(iele,1:nnod_4_e)
            xe(1:nnod_4_e) =  xx(inod(1:nnod_4_e), 1)
            dxi(1:nnod_4_e) = xe(1:nnod_4_e) * dnxi(1:nnod_4_e,ix)
            dxdxi_ele(iele) = dxdxi_ele(iele) + sum(dxi) * owe3d(ix)
!
            dxi(1:nnod_4_e) = xe(1:nnod_4_e) * dnei(1:nnod_4_e,ix)
            dxdei_ele(iele) = dxdei_ele(iele) + sum(dxi) * owe3d(ix)
!
            dxi(1:nnod_4_e) = xe(1:nnod_4_e) * dnzi(1:nnod_4_e,ix)
            dxdei_ele(iele) = dxdei_ele(iele) + sum(dxi) * owe3d(ix)
!
!
            xe(1:nnod_4_e) =  xx(inod(1:nnod_4_e), 2)
            dxi(1:nnod_4_e) = xe(1:nnod_4_e) * dnxi(1:nnod_4_e,ix)
            dydxi_ele(iele) = dydxi_ele(iele) + sum(dxi) * owe3d(ix)
!
            dxi(1:nnod_4_e) = xe(1:nnod_4_e) * dnei(1:nnod_4_e,ix)
            dydei_ele(iele) = dydei_ele(iele) + sum(dxi) * owe3d(ix)
!
            dxi(1:nnod_4_e) = xe(1:nnod_4_e) * dnzi(1:nnod_4_e,ix)
            dydzi_ele(iele) = dydzi_ele(iele) + sum(dxi) * owe3d(ix)
!
!
            xe(1:nnod_4_e) =  xx(inod(1:nnod_4_e), 3)
            dxi(1:nnod_4_e) = xe(1:nnod_4_e) * dnxi(1:nnod_4_e,ix)
            dzdxi_ele(iele) = dzdxi_ele(iele) + sum(dxi) * owe3d(ix)
!
            dxi(1:nnod_4_e) = xe(1:nnod_4_e) * dnei(1:nnod_4_e,ix)
            dzdei_ele(iele) = dzdei_ele(iele) + sum(dxi) * owe3d(ix)
!
            dxi(1:nnod_4_e) = xe(1:nnod_4_e) * dnzi(1:nnod_4_e,ix)
            dzdzi_ele(iele) = dzdzi_ele(iele) + sum(dxi) * owe3d(ix)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine fem_element_length
!
!-----------------------------------------------------------------------
!
      end module int_element_length
