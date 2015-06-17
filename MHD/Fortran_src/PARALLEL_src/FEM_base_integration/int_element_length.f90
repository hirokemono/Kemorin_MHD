!
!     module int_element_length
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        Modified by H. Matsui on Aug., 2006
!
!      subroutine s_int_element_length
!
      module int_element_length
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      implicit none
!
!   minimum and maximum of element position
!
      private :: s_int_element_length
      private :: fem_element_length_linear, fem_element_length_quad
      private :: fem_element_length_lag, cal_element_length_by_jacobi
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_element_length_1st
!
      use m_filter_elength
      use m_filter_dxdxi
!
      call s_int_element_length(nele_filter_mom,                        &
     &    filter_dxi1%dxi_ele, elen1%moms)
!
      end subroutine int_element_length_1st
!
!-----------------------------------------------------------------------
!
      subroutine s_int_element_length(nele_filter, dxi_ele, elen_ele)
!
      use m_geometry_constants
      use m_geometry_parameter
      use m_fem_gauss_int_coefs
      use t_filter_dxdxi
      use t_filter_elength
!
!
      integer(kind = kint), intent(in) :: nele_filter
      type(dxdxi_direction_type), intent(inout) :: dxi_ele
      type(elen_on_ele_type), intent(inout) :: elen_ele
!
!
      if      (nnod_4_ele .eq. num_t_linear) then
        call fem_element_length_linear(max_int_point, nele_filter,      &
     &      dxi_ele%dx%df_dxi, dxi_ele%dx%df_dei, dxi_ele%dx%df_dzi,    &
     &      dxi_ele%dy%df_dxi, dxi_ele%dy%df_dei, dxi_ele%dy%df_dzi,    &
     &      dxi_ele%dz%df_dxi, dxi_ele%dz%df_dei, dxi_ele%dz%df_dzi)
      else if (nnod_4_ele .eq. num_t_quad) then
        call fem_element_length_quad(max_int_point, nele_filter,        &
     &      dxi_ele%dx%df_dxi, dxi_ele%dx%df_dei, dxi_ele%dx%df_dzi,    &
     &      dxi_ele%dy%df_dxi, dxi_ele%dy%df_dei, dxi_ele%dy%df_dzi,    &
     &      dxi_ele%dz%df_dxi, dxi_ele%dz%df_dei, dxi_ele%dz%df_dzi)
      else if (nnod_4_ele .eq. num_t_lag) then
        call fem_element_length_lag(max_int_point, nele_filter,         &
     &      dxi_ele%dx%df_dxi, dxi_ele%dx%df_dei, dxi_ele%dx%df_dzi,    &
     &      dxi_ele%dy%df_dxi, dxi_ele%dy%df_dei, dxi_ele%dy%df_dzi,    &
     &      dxi_ele%dz%df_dxi, dxi_ele%dz%df_dei, dxi_ele%dz%df_dzi)
      end if
!
      call cal_element_length_by_jacobi(nele_filter,                    &
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
      subroutine cal_element_length_by_jacobi(nele_filter,              &
     &          dxdxi_ele, dxdei_ele, dxdzi_ele,                        &
     &          dydxi_ele, dydei_ele, dydzi_ele,                        &
     &          dzdxi_ele, dzdei_ele, dzdzi_ele,                        &
     &          elen_dx2_ele,  elen_dy2_ele,  elen_dz2_ele,             &
     &          elen_dxdy_ele, elen_dydz_ele, elen_dzdx_ele)
!
      use m_geometry_parameter
      use m_shape_functions
!
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
!
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
      subroutine fem_element_length_linear(n_int, nele_filter,          &
     &          dxdxi_ele, dxdei_ele, dxdzi_ele,                        &
     &          dydxi_ele, dydei_ele, dydzi_ele,                        &
     &          dzdxi_ele, dzdei_ele, dzdzi_ele)
!
      use m_geometry_parameter
      use m_machine_parameter
      use m_geometry_data
      use m_fem_gauss_int_coefs
      use m_shape_functions
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
      integer(kind = kint) :: inod1,  inod2,  inod3,  inod4,  inod5
      integer(kind = kint) :: inod6,  inod7,  inod8
!
!
!$omp parallel do private(iele,ii,ix,ist,ied,inod1,inod2,inod3,         &
!$omp&                    inod4,inod5,inod6,inod7,inod8)
      do iproc = 1, np_smp
!
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
            inod1  = ie(iele, 1)
            inod2  = ie(iele, 2)
            inod3  = ie(iele, 3)
            inod4  = ie(iele, 4)
            inod5  = ie(iele, 5)
            inod6  = ie(iele, 6)
            inod7  = ie(iele, 7)
            inod8  = ie(iele, 8)
!
            dxdxi_ele(iele) = dxdxi_ele(iele)                           &
     &                      + ( xx(inod1, 1) * dnxi_1(1, ix)            &
     &                        + xx(inod2, 1) * dnxi_1(2, ix)            &
     &                        + xx(inod3, 1) * dnxi_1(3, ix)            &
     &                        + xx(inod4, 1) * dnxi_1(4, ix)            &
     &                        + xx(inod5, 1) * dnxi_1(5, ix)            &
     &                        + xx(inod6, 1) * dnxi_1(6, ix)            &
     &                        + xx(inod7, 1) * dnxi_1(7, ix)            &
     &                        + xx(inod8, 1) * dnxi_1(8, ix)            &
     &                         )  * owe3d(ix)
!
            dxdei_ele(iele) = dxdei_ele(iele)                           &
     &                      + ( xx(inod1, 1) * dnei_1(1, ix)            &
     &                        + xx(inod2, 1) * dnei_1(2, ix)            &
     &                        + xx(inod3, 1) * dnei_1(3, ix)            &
     &                        + xx(inod4, 1) * dnei_1(4, ix)            &
     &                        + xx(inod5, 1) * dnei_1(5, ix)            &
     &                        + xx(inod6, 1) * dnei_1(6, ix)            &
     &                        + xx(inod7, 1) * dnei_1(7, ix)            &
     &                        + xx(inod8, 1) * dnei_1(8, ix)            &
     &                         )  * owe3d(ix)
!
            dxdzi_ele(iele) = dxdzi_ele(iele)                           &
     &                      + ( xx(inod1, 1) * dnzi_1(1, ix)            &
     &                        + xx(inod2, 1) * dnzi_1(2, ix)            &
     &                        + xx(inod3, 1) * dnzi_1(3, ix)            &
     &                        + xx(inod4, 1) * dnzi_1(4, ix)            &
     &                        + xx(inod5, 1) * dnzi_1(5, ix)            &
     &                        + xx(inod6, 1) * dnzi_1(6, ix)            &
     &                        + xx(inod7, 1) * dnzi_1(7, ix)            &
     &                        + xx(inod8, 1) * dnzi_1(8, ix)            &
     &                         )  * owe3d(ix)
!
!
            dydxi_ele(iele) = dydxi_ele(iele)                           &
     &                      + ( xx(inod1, 2) * dnxi_1(1, ix)            &
     &                        + xx(inod2, 2) * dnxi_1(2, ix)            &
     &                        + xx(inod3, 2) * dnxi_1(3, ix)            &
     &                        + xx(inod4, 2) * dnxi_1(4, ix)            &
     &                        + xx(inod5, 2) * dnxi_1(5, ix)            &
     &                        + xx(inod6, 2) * dnxi_1(6, ix)            &
     &                        + xx(inod7, 2) * dnxi_1(7, ix)            &
     &                        + xx(inod8, 2) * dnxi_1(8, ix)            &
     &                         )  * owe3d(ix)
!
            dydei_ele(iele) = dydei_ele(iele)                           &
     &                      + ( xx(inod1, 2) * dnei_1(1, ix)            &
     &                        + xx(inod2, 2) * dnei_1(2, ix)            &
     &                        + xx(inod3, 2) * dnei_1(3, ix)            &
     &                        + xx(inod4, 2) * dnei_1(4, ix)            &
     &                        + xx(inod5, 2) * dnei_1(5, ix)            &
     &                        + xx(inod6, 2) * dnei_1(6, ix)            &
     &                        + xx(inod7, 2) * dnei_1(7, ix)            &
     &                        + xx(inod8, 2) * dnei_1(8, ix)            &
     &                         )  * owe3d(ix)
!
            dydzi_ele(iele) = dydzi_ele(iele)                           &
     &                      + ( xx(inod1, 2) * dnzi_1(1, ix)            &
     &                        + xx(inod2, 2) * dnzi_1(2, ix)            &
     &                        + xx(inod3, 2) * dnzi_1(3, ix)            &
     &                        + xx(inod4, 2) * dnzi_1(4, ix)            &
     &                        + xx(inod5, 2) * dnzi_1(5, ix)            &
     &                        + xx(inod6, 2) * dnzi_1(6, ix)            &
     &                        + xx(inod7, 2) * dnzi_1(7, ix)            &
     &                        + xx(inod8, 2) * dnzi_1(8, ix)            &
     &                         )  * owe3d(ix)
!
!
            dzdxi_ele(iele) = dzdxi_ele(iele)                           &
     &                      + ( xx(inod1, 3) * dnxi_1(1, ix)            &
     &                        + xx(inod2, 3) * dnxi_1(2, ix)            &
     &                        + xx(inod3, 3) * dnxi_1(3, ix)            &
     &                        + xx(inod4, 3) * dnxi_1(4, ix)            &
     &                        + xx(inod5, 3) * dnxi_1(5, ix)            &
     &                        + xx(inod6, 3) * dnxi_1(6, ix)            &
     &                        + xx(inod7, 3) * dnxi_1(7, ix)            &
     &                        + xx(inod8, 3) * dnxi_1(8, ix)            &
     &                         )  * owe3d(ix)
!
            dzdei_ele(iele) = dzdei_ele(iele)                           &
     &                      + ( xx(inod1, 3) * dnei_1(1, ix)            &
     &                        + xx(inod2, 3) * dnei_1(2, ix)            &
     &                        + xx(inod3, 3) * dnei_1(3, ix)            &
     &                        + xx(inod4, 3) * dnei_1(4, ix)            &
     &                        + xx(inod5, 3) * dnei_1(5, ix)            &
     &                        + xx(inod6, 3) * dnei_1(6, ix)            &
     &                        + xx(inod7, 3) * dnei_1(7, ix)            &
     &                        + xx(inod8, 3) * dnei_1(8, ix)            &
     &                         )  * owe3d(ix)
!
            dzdzi_ele(iele) = dzdzi_ele(iele)                           &
     &                      + ( xx(inod1, 3) * dnzi_1(1, ix)            &
     &                        + xx(inod2, 3) * dnzi_1(2, ix)            &
     &                        + xx(inod3, 3) * dnzi_1(3, ix)            &
     &                        + xx(inod4, 3) * dnzi_1(4, ix)            &
     &                        + xx(inod5, 3) * dnzi_1(5, ix)            &
     &                        + xx(inod6, 3) * dnzi_1(6, ix)            &
     &                        + xx(inod7, 3) * dnzi_1(7, ix)            &
     &                        + xx(inod8, 3) * dnzi_1(8, ix)            &
     &                         )  * owe3d(ix)
!
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine fem_element_length_linear
!
!-----------------------------------------------------------------------
!
      subroutine fem_element_length_quad(n_int, nele_filter,            &
     &          dxdxi_ele, dxdei_ele, dxdzi_ele,                        &
     &          dydxi_ele, dydei_ele, dydzi_ele,                        &
     &          dzdxi_ele, dzdei_ele, dzdzi_ele)
!
      use m_geometry_parameter
      use m_machine_parameter
      use m_geometry_data
      use m_fem_gauss_int_coefs
      use m_shape_functions
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
      integer(kind = kint) :: inod1,  inod2,  inod3,  inod4,  inod5
      integer(kind = kint) :: inod6,  inod7,  inod8,  inod9,  inod10
      integer(kind = kint) :: inod11, inod12, inod13, inod14, inod15
      integer(kind = kint) :: inod16, inod17, inod18, inod19, inod20
!
!
!$omp parallel do private(iele,ii,ix,ist,ied,inod1,inod2,inod3,         &
!$omp&                    inod4,inod5,inod6,inod7,inod8,inod9,inod10,   &
!$omp&                    inod11,inod12,inod13,inod14,inod15,inod16,    &
!$omp&                    inod17,inod18,inod19,inod20)
      do iproc = 1, np_smp
!
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
            inod1  = ie(iele, 1)
            inod2  = ie(iele, 2)
            inod3  = ie(iele, 3)
            inod4  = ie(iele, 4)
            inod5  = ie(iele, 5)
            inod6  = ie(iele, 6)
            inod7  = ie(iele, 7)
            inod8  = ie(iele, 8)
            inod9  = ie(iele, 9)
            inod10 = ie(iele,10)
            inod11 = ie(iele,11)
            inod12 = ie(iele,12)
            inod13 = ie(iele,13)
            inod14 = ie(iele,14)
            inod15 = ie(iele,15)
            inod16 = ie(iele,16)
            inod17 = ie(iele,17)
            inod18 = ie(iele,18)
            inod19 = ie(iele,19)
            inod20 = ie(iele,20)
!
            dxdxi_ele(iele) = dxdxi_ele(iele)                           &
     &                      + ( xx(inod1, 1) * dnxi_20(1, ix)           &
     &                        + xx(inod2, 1) * dnxi_20(2, ix)           &
     &                        + xx(inod3, 1) * dnxi_20(3, ix)           &
     &                        + xx(inod4, 1) * dnxi_20(4, ix)           &
     &                        + xx(inod5, 1) * dnxi_20(5, ix)           &
     &                        + xx(inod6, 1) * dnxi_20(6, ix)           &
     &                        + xx(inod7, 1) * dnxi_20(7, ix)           &
     &                        + xx(inod8, 1) * dnxi_20(8, ix)           &
     &                        + xx(inod9, 1) * dnxi_20(9, ix)           &
     &                        + xx(inod10,1) * dnxi_20(10,ix)           &
     &                        + xx(inod11,1) * dnxi_20(11,ix)           &
     &                        + xx(inod12,1) * dnxi_20(12,ix)           &
     &                        + xx(inod13,1) * dnxi_20(13,ix)           &
     &                        + xx(inod14,1) * dnxi_20(14,ix)           &
     &                        + xx(inod15,1) * dnxi_20(15,ix)           &
     &                        + xx(inod16,1) * dnxi_20(16,ix)           &
     &                        + xx(inod17,1) * dnxi_20(17,ix)           &
     &                        + xx(inod18,1) * dnxi_20(18,ix)           &
     &                        + xx(inod19,1) * dnxi_20(19,ix)           &
     &                        + xx(inod20,1) * dnxi_20(20,ix)           &
     &                         )  * owe3d(ix)
!
            dxdei_ele(iele) = dxdei_ele(iele)                           &
     &                      + ( xx(inod1, 1) * dnei_20(1, ix)           &
     &                        + xx(inod2, 1) * dnei_20(2, ix)           &
     &                        + xx(inod3, 1) * dnei_20(3, ix)           &
     &                        + xx(inod4, 1) * dnei_20(4, ix)           &
     &                        + xx(inod5, 1) * dnei_20(5, ix)           &
     &                        + xx(inod6, 1) * dnei_20(6, ix)           &
     &                        + xx(inod7, 1) * dnei_20(7, ix)           &
     &                        + xx(inod8, 1) * dnei_20(8, ix)           &
     &                        + xx(inod9, 1) * dnei_20(9, ix)           &
     &                        + xx(inod10,1) * dnei_20(10,ix)           &
     &                        + xx(inod11,1) * dnei_20(11,ix)           &
     &                        + xx(inod12,1) * dnei_20(12,ix)           &
     &                        + xx(inod13,1) * dnei_20(13,ix)           &
     &                        + xx(inod14,1) * dnei_20(14,ix)           &
     &                        + xx(inod15,1) * dnei_20(15,ix)           &
     &                        + xx(inod16,1) * dnei_20(16,ix)           &
     &                        + xx(inod17,1) * dnei_20(17,ix)           &
     &                        + xx(inod18,1) * dnei_20(18,ix)           &
     &                        + xx(inod19,1) * dnei_20(19,ix)           &
     &                        + xx(inod20,1) * dnei_20(20,ix)           &
     &                         )  * owe3d(ix)
!
            dxdzi_ele(iele) = dxdzi_ele(iele)                           &
     &                      + ( xx(inod1, 1) * dnzi_20(1, ix)           &
     &                        + xx(inod2, 1) * dnzi_20(2, ix)           &
     &                        + xx(inod3, 1) * dnzi_20(3, ix)           &
     &                        + xx(inod4, 1) * dnzi_20(4, ix)           &
     &                        + xx(inod5, 1) * dnzi_20(5, ix)           &
     &                        + xx(inod6, 1) * dnzi_20(6, ix)           &
     &                        + xx(inod7, 1) * dnzi_20(7, ix)           &
     &                        + xx(inod8, 1) * dnzi_20(8, ix)           &
     &                        + xx(inod9, 1) * dnzi_20(9, ix)           &
     &                        + xx(inod10,1) * dnzi_20(10,ix)           &
     &                        + xx(inod11,1) * dnzi_20(11,ix)           &
     &                        + xx(inod12,1) * dnzi_20(12,ix)           &
     &                        + xx(inod13,1) * dnzi_20(13,ix)           &
     &                        + xx(inod14,1) * dnzi_20(14,ix)           &
     &                        + xx(inod15,1) * dnzi_20(15,ix)           &
     &                        + xx(inod16,1) * dnzi_20(16,ix)           &
     &                        + xx(inod17,1) * dnzi_20(17,ix)           &
     &                        + xx(inod18,1) * dnzi_20(18,ix)           &
     &                        + xx(inod19,1) * dnzi_20(19,ix)           &
     &                        + xx(inod20,1) * dnzi_20(20,ix)           &
     &                         )  * owe3d(ix)
!
!
            dydxi_ele(iele) = dydxi_ele(iele)                           &
     &                      + ( xx(inod1, 2) * dnxi_20(1, ix)           &
     &                        + xx(inod2, 2) * dnxi_20(2, ix)           &
     &                        + xx(inod3, 2) * dnxi_20(3, ix)           &
     &                        + xx(inod4, 2) * dnxi_20(4, ix)           &
     &                        + xx(inod5, 2) * dnxi_20(5, ix)           &
     &                        + xx(inod6, 2) * dnxi_20(6, ix)           &
     &                        + xx(inod7, 2) * dnxi_20(7, ix)           &
     &                        + xx(inod8, 2) * dnxi_20(8, ix)           &
     &                        + xx(inod9, 2) * dnxi_20(9, ix)           &
     &                        + xx(inod10,2) * dnxi_20(10,ix)           &
     &                        + xx(inod11,2) * dnxi_20(11,ix)           &
     &                        + xx(inod12,2) * dnxi_20(12,ix)           &
     &                        + xx(inod13,2) * dnxi_20(13,ix)           &
     &                        + xx(inod14,2) * dnxi_20(14,ix)           &
     &                        + xx(inod15,2) * dnxi_20(15,ix)           &
     &                        + xx(inod16,2) * dnxi_20(16,ix)           &
     &                        + xx(inod17,2) * dnxi_20(17,ix)           &
     &                        + xx(inod18,2) * dnxi_20(18,ix)           &
     &                        + xx(inod19,2) * dnxi_20(19,ix)           &
     &                        + xx(inod20,2) * dnxi_20(20,ix)           &
     &                         )  * owe3d(ix)
!
            dydei_ele(iele) = dydei_ele(iele)                           &
     &                      + ( xx(inod1, 2) * dnei_20(1, ix)           &
     &                        + xx(inod2, 2) * dnei_20(2, ix)           &
     &                        + xx(inod3, 2) * dnei_20(3, ix)           &
     &                        + xx(inod4, 2) * dnei_20(4, ix)           &
     &                        + xx(inod5, 2) * dnei_20(5, ix)           &
     &                        + xx(inod6, 2) * dnei_20(6, ix)           &
     &                        + xx(inod7, 2) * dnei_20(7, ix)           &
     &                        + xx(inod8, 2) * dnei_20(8, ix)           &
     &                        + xx(inod9, 2) * dnei_20(9, ix)           &
     &                        + xx(inod10,2) * dnei_20(10,ix)           &
     &                        + xx(inod11,2) * dnei_20(11,ix)           &
     &                        + xx(inod12,2) * dnei_20(12,ix)           &
     &                        + xx(inod13,2) * dnei_20(13,ix)           &
     &                        + xx(inod14,2) * dnei_20(14,ix)           &
     &                        + xx(inod15,2) * dnei_20(15,ix)           &
     &                        + xx(inod16,2) * dnei_20(16,ix)           &
     &                        + xx(inod17,2) * dnei_20(17,ix)           &
     &                        + xx(inod18,2) * dnei_20(18,ix)           &
     &                        + xx(inod19,2) * dnei_20(19,ix)           &
     &                        + xx(inod20,2) * dnei_20(20,ix)           &
     &                         )  * owe3d(ix)
!
            dydzi_ele(iele) = dydzi_ele(iele)                           &
     &                      + ( xx(inod1, 2) * dnzi_20(1, ix)           &
     &                        + xx(inod2, 2) * dnzi_20(2, ix)           &
     &                        + xx(inod3, 2) * dnzi_20(3, ix)           &
     &                        + xx(inod4, 2) * dnzi_20(4, ix)           &
     &                        + xx(inod5, 2) * dnzi_20(5, ix)           &
     &                        + xx(inod6, 2) * dnzi_20(6, ix)           &
     &                        + xx(inod7, 2) * dnzi_20(7, ix)           &
     &                        + xx(inod8, 2) * dnzi_20(8, ix)           &
     &                        + xx(inod9, 2) * dnzi_20(9, ix)           &
     &                        + xx(inod10,2) * dnzi_20(10,ix)           &
     &                        + xx(inod11,2) * dnzi_20(11,ix)           &
     &                        + xx(inod12,2) * dnzi_20(12,ix)           &
     &                        + xx(inod13,2) * dnzi_20(13,ix)           &
     &                        + xx(inod14,2) * dnzi_20(14,ix)           &
     &                        + xx(inod15,2) * dnzi_20(15,ix)           &
     &                        + xx(inod16,2) * dnzi_20(16,ix)           &
     &                        + xx(inod17,2) * dnzi_20(17,ix)           &
     &                        + xx(inod18,2) * dnzi_20(18,ix)           &
     &                        + xx(inod19,2) * dnzi_20(19,ix)           &
     &                        + xx(inod20,2) * dnzi_20(20,ix)           &
     &                         )  * owe3d(ix)
!
!
            dzdxi_ele(iele) = dzdxi_ele(iele)                           &
     &                      + ( xx(inod1, 3) * dnxi_20(1, ix)           &
     &                        + xx(inod2, 3) * dnxi_20(2, ix)           &
     &                        + xx(inod3, 3) * dnxi_20(3, ix)           &
     &                        + xx(inod4, 3) * dnxi_20(4, ix)           &
     &                        + xx(inod5, 3) * dnxi_20(5, ix)           &
     &                        + xx(inod6, 3) * dnxi_20(6, ix)           &
     &                        + xx(inod7, 3) * dnxi_20(7, ix)           &
     &                        + xx(inod8, 3) * dnxi_20(8, ix)           &
     &                        + xx(inod9, 3) * dnxi_20(9, ix)           &
     &                        + xx(inod10,3) * dnxi_20(10,ix)           &
     &                        + xx(inod11,3) * dnxi_20(11,ix)           &
     &                        + xx(inod12,3) * dnxi_20(12,ix)           &
     &                        + xx(inod13,3) * dnxi_20(13,ix)           &
     &                        + xx(inod14,3) * dnxi_20(14,ix)           &
     &                        + xx(inod15,3) * dnxi_20(15,ix)           &
     &                        + xx(inod16,3) * dnxi_20(16,ix)           &
     &                        + xx(inod17,3) * dnxi_20(17,ix)           &
     &                        + xx(inod18,3) * dnxi_20(18,ix)           &
     &                        + xx(inod19,3) * dnxi_20(19,ix)           &
     &                        + xx(inod20,3) * dnxi_20(20,ix)           &
     &                         )  * owe3d(ix)
!
            dzdei_ele(iele) = dzdei_ele(iele)                           &
     &                      + ( xx(inod1, 3) * dnei_20(1, ix)           &
     &                        + xx(inod2, 3) * dnei_20(2, ix)           &
     &                        + xx(inod3, 3) * dnei_20(3, ix)           &
     &                        + xx(inod4, 3) * dnei_20(4, ix)           &
     &                        + xx(inod5, 3) * dnei_20(5, ix)           &
     &                        + xx(inod6, 3) * dnei_20(6, ix)           &
     &                        + xx(inod7, 3) * dnei_20(7, ix)           &
     &                        + xx(inod8, 3) * dnei_20(8, ix)           &
     &                        + xx(inod9, 3) * dnei_20(9, ix)           &
     &                        + xx(inod10,3) * dnei_20(10,ix)           &
     &                        + xx(inod11,3) * dnei_20(11,ix)           &
     &                        + xx(inod12,3) * dnei_20(12,ix)           &
     &                        + xx(inod13,3) * dnei_20(13,ix)           &
     &                        + xx(inod14,3) * dnei_20(14,ix)           &
     &                        + xx(inod15,3) * dnei_20(15,ix)           &
     &                        + xx(inod16,3) * dnei_20(16,ix)           &
     &                        + xx(inod17,3) * dnei_20(17,ix)           &
     &                        + xx(inod18,3) * dnei_20(18,ix)           &
     &                        + xx(inod19,3) * dnei_20(19,ix)           &
     &                        + xx(inod20,3) * dnei_20(20,ix)           &
     &                         )  * owe3d(ix)
!
            dzdzi_ele(iele) = dzdzi_ele(iele)                           &
     &                      + ( xx(inod1, 3) * dnzi_20(1, ix)           &
     &                        + xx(inod2, 3) * dnzi_20(2, ix)           &
     &                        + xx(inod3, 3) * dnzi_20(3, ix)           &
     &                        + xx(inod4, 3) * dnzi_20(4, ix)           &
     &                        + xx(inod5, 3) * dnzi_20(5, ix)           &
     &                        + xx(inod6, 3) * dnzi_20(6, ix)           &
     &                        + xx(inod7, 3) * dnzi_20(7, ix)           &
     &                        + xx(inod8, 3) * dnzi_20(8, ix)           &
     &                        + xx(inod9, 3) * dnzi_20(9, ix)           &
     &                        + xx(inod10,3) * dnzi_20(10,ix)           &
     &                        + xx(inod11,3) * dnzi_20(11,ix)           &
     &                        + xx(inod12,3) * dnzi_20(12,ix)           &
     &                        + xx(inod13,3) * dnzi_20(13,ix)           &
     &                        + xx(inod14,3) * dnzi_20(14,ix)           &
     &                        + xx(inod15,3) * dnzi_20(15,ix)           &
     &                        + xx(inod16,3) * dnzi_20(16,ix)           &
     &                        + xx(inod17,3) * dnzi_20(17,ix)           &
     &                        + xx(inod18,3) * dnzi_20(18,ix)           &
     &                        + xx(inod19,3) * dnzi_20(19,ix)           &
     &                        + xx(inod20,3) * dnzi_20(20,ix)           &
     &                         )  * owe3d(ix)
!
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine fem_element_length_quad
!
!-----------------------------------------------------------------------
!
      subroutine fem_element_length_lag(n_int, nele_filter,             &
     &          dxdxi_ele, dxdei_ele, dxdzi_ele,                        &
     &          dydxi_ele, dydei_ele, dydzi_ele,                        &
     &          dzdxi_ele, dzdei_ele, dzdzi_ele)
!
      use m_geometry_parameter
      use m_machine_parameter
      use m_geometry_data
      use m_fem_gauss_int_coefs
      use m_shape_functions
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
      integer(kind = kint) :: inod1,  inod2,  inod3,  inod4,  inod5
      integer(kind = kint) :: inod6,  inod7,  inod8,  inod9,  inod10
      integer(kind = kint) :: inod11, inod12, inod13, inod14, inod15
      integer(kind = kint) :: inod16, inod17, inod18, inod19, inod20
      integer(kind = kint) :: inod21, inod22, inod23, inod24, inod25
      integer(kind = kint) :: inod26, inod27
!
!
!$omp parallel do private(iele,ii,ix,ist,ied,inod1,inod2,inod3,         &
!$omp&                    inod4,inod5,inod6,inod7,inod8,inod9,inod10,   &
!$omp&                    inod11,inod12,inod13,inod14,inod15,inod16,    &
!$omp&                    inod17,inod18,inod19,inod20,inod21,inod22,    &
!$omp&                    inod23,inod24,inod25,inod26,inod27)
      do iproc = 1, np_smp
!
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
            inod1  = ie(iele, 1)
            inod2  = ie(iele, 2)
            inod3  = ie(iele, 3)
            inod4  = ie(iele, 4)
            inod5  = ie(iele, 5)
            inod6  = ie(iele, 6)
            inod7  = ie(iele, 7)
            inod8  = ie(iele, 8)
            inod9  = ie(iele, 9)
            inod10 = ie(iele,10)
            inod11 = ie(iele,11)
            inod12 = ie(iele,12)
            inod13 = ie(iele,13)
            inod14 = ie(iele,14)
            inod15 = ie(iele,15)
            inod16 = ie(iele,16)
            inod17 = ie(iele,17)
            inod18 = ie(iele,18)
            inod19 = ie(iele,19)
            inod20 = ie(iele,20)
            inod21 = ie(iele,21)
            inod22 = ie(iele,22)
            inod23 = ie(iele,23)
            inod24 = ie(iele,24)
            inod25 = ie(iele,25)
            inod26 = ie(iele,26)
            inod27 = ie(iele,27)
!
            dxdxi_ele(iele) = dxdxi_ele(iele)                           &
     &                      + ( xx(inod1, 1) * dnxi_27(1, ix)           &
     &                        + xx(inod2, 1) * dnxi_27(2, ix)           &
     &                        + xx(inod3, 1) * dnxi_27(3, ix)           &
     &                        + xx(inod4, 1) * dnxi_27(4, ix)           &
     &                        + xx(inod5, 1) * dnxi_27(5, ix)           &
     &                        + xx(inod6, 1) * dnxi_27(6, ix)           &
     &                        + xx(inod7, 1) * dnxi_27(7, ix)           &
     &                        + xx(inod8, 1) * dnxi_27(8, ix)           &
     &                        + xx(inod9, 1) * dnxi_27(9, ix)           &
     &                        + xx(inod10,1) * dnxi_27(10,ix)           &
     &                        + xx(inod11,1) * dnxi_27(11,ix)           &
     &                        + xx(inod12,1) * dnxi_27(12,ix)           &
     &                        + xx(inod13,1) * dnxi_27(13,ix)           &
     &                        + xx(inod14,1) * dnxi_27(14,ix)           &
     &                        + xx(inod15,1) * dnxi_27(15,ix)           &
     &                        + xx(inod16,1) * dnxi_27(16,ix)           &
     &                        + xx(inod17,1) * dnxi_27(17,ix)           &
     &                        + xx(inod18,1) * dnxi_27(18,ix)           &
     &                        + xx(inod19,1) * dnxi_27(19,ix)           &
     &                        + xx(inod20,1) * dnxi_27(20,ix)           &
     &                        + xx(inod21,1) * dnxi_27(21,ix)           &
     &                        + xx(inod22,1) * dnxi_27(22,ix)           &
     &                        + xx(inod23,1) * dnxi_27(23,ix)           &
     &                        + xx(inod24,1) * dnxi_27(24,ix)           &
     &                        + xx(inod25,1) * dnxi_27(25,ix)           &
     &                        + xx(inod26,1) * dnxi_27(26,ix)           &
     &                        + xx(inod27,1) * dnxi_27(27,ix)           &
     &                         )  * owe3d(ix)
!
            dxdei_ele(iele) = dxdei_ele(iele)                           &
     &                      + ( xx(inod1, 1) * dnei_27(1, ix)           &
     &                        + xx(inod2, 1) * dnei_27(2, ix)           &
     &                        + xx(inod3, 1) * dnei_27(3, ix)           &
     &                        + xx(inod4, 1) * dnei_27(4, ix)           &
     &                        + xx(inod5, 1) * dnei_27(5, ix)           &
     &                        + xx(inod6, 1) * dnei_27(6, ix)           &
     &                        + xx(inod7, 1) * dnei_27(7, ix)           &
     &                        + xx(inod8, 1) * dnei_27(8, ix)           &
     &                        + xx(inod9, 1) * dnei_27(9, ix)           &
     &                        + xx(inod10,1) * dnei_27(10,ix)           &
     &                        + xx(inod11,1) * dnei_27(11,ix)           &
     &                        + xx(inod12,1) * dnei_27(12,ix)           &
     &                        + xx(inod13,1) * dnei_27(13,ix)           &
     &                        + xx(inod14,1) * dnei_27(14,ix)           &
     &                        + xx(inod15,1) * dnei_27(15,ix)           &
     &                        + xx(inod16,1) * dnei_27(16,ix)           &
     &                        + xx(inod17,1) * dnei_27(17,ix)           &
     &                        + xx(inod18,1) * dnei_27(18,ix)           &
     &                        + xx(inod19,1) * dnei_27(19,ix)           &
     &                        + xx(inod20,1) * dnei_27(20,ix)           &
     &                        + xx(inod21,1) * dnei_27(21,ix)           &
     &                        + xx(inod22,1) * dnei_27(22,ix)           &
     &                        + xx(inod23,1) * dnei_27(23,ix)           &
     &                        + xx(inod24,1) * dnei_27(24,ix)           &
     &                        + xx(inod25,1) * dnei_27(25,ix)           &
     &                        + xx(inod26,1) * dnei_27(26,ix)           &
     &                        + xx(inod27,1) * dnei_27(27,ix)           &
     &                         )  * owe3d(ix)
!
            dxdzi_ele(iele) = dxdzi_ele(iele)                           &
     &                      + ( xx(inod1, 1) * dnzi_27(1, ix)           &
     &                        + xx(inod2, 1) * dnzi_27(2, ix)           &
     &                        + xx(inod3, 1) * dnzi_27(3, ix)           &
     &                        + xx(inod4, 1) * dnzi_27(4, ix)           &
     &                        + xx(inod5, 1) * dnzi_27(5, ix)           &
     &                        + xx(inod6, 1) * dnzi_27(6, ix)           &
     &                        + xx(inod7, 1) * dnzi_27(7, ix)           &
     &                        + xx(inod8, 1) * dnzi_27(8, ix)           &
     &                        + xx(inod9, 1) * dnzi_27(9, ix)           &
     &                        + xx(inod10,1) * dnzi_27(10,ix)           &
     &                        + xx(inod11,1) * dnzi_27(11,ix)           &
     &                        + xx(inod12,1) * dnzi_27(12,ix)           &
     &                        + xx(inod13,1) * dnzi_27(13,ix)           &
     &                        + xx(inod14,1) * dnzi_27(14,ix)           &
     &                        + xx(inod15,1) * dnzi_27(15,ix)           &
     &                        + xx(inod16,1) * dnzi_27(16,ix)           &
     &                        + xx(inod17,1) * dnzi_27(17,ix)           &
     &                        + xx(inod18,1) * dnzi_27(18,ix)           &
     &                        + xx(inod19,1) * dnzi_27(19,ix)           &
     &                        + xx(inod20,1) * dnzi_27(20,ix)           &
     &                        + xx(inod21,1) * dnzi_27(21,ix)           &
     &                        + xx(inod22,1) * dnzi_27(22,ix)           &
     &                        + xx(inod23,1) * dnzi_27(23,ix)           &
     &                        + xx(inod24,1) * dnzi_27(24,ix)           &
     &                        + xx(inod25,1) * dnzi_27(25,ix)           &
     &                        + xx(inod26,1) * dnzi_27(26,ix)           &
     &                        + xx(inod27,1) * dnzi_27(27,ix)           &
     &                         )  * owe3d(ix)
!
!
            dydxi_ele(iele) = dydxi_ele(iele)                           &
     &                      + ( xx(inod1, 2) * dnxi_27(1, ix)           &
     &                        + xx(inod2, 2) * dnxi_27(2, ix)           &
     &                        + xx(inod3, 2) * dnxi_27(3, ix)           &
     &                        + xx(inod4, 2) * dnxi_27(4, ix)           &
     &                        + xx(inod5, 2) * dnxi_27(5, ix)           &
     &                        + xx(inod6, 2) * dnxi_27(6, ix)           &
     &                        + xx(inod7, 2) * dnxi_27(7, ix)           &
     &                        + xx(inod8, 2) * dnxi_27(8, ix)           &
     &                        + xx(inod9, 2) * dnxi_27(9, ix)           &
     &                        + xx(inod10,2) * dnxi_27(10,ix)           &
     &                        + xx(inod11,2) * dnxi_27(11,ix)           &
     &                        + xx(inod12,2) * dnxi_27(12,ix)           &
     &                        + xx(inod13,2) * dnxi_27(13,ix)           &
     &                        + xx(inod14,2) * dnxi_27(14,ix)           &
     &                        + xx(inod15,2) * dnxi_27(15,ix)           &
     &                        + xx(inod16,2) * dnxi_27(16,ix)           &
     &                        + xx(inod17,2) * dnxi_27(17,ix)           &
     &                        + xx(inod18,2) * dnxi_27(18,ix)           &
     &                        + xx(inod19,2) * dnxi_27(19,ix)           &
     &                        + xx(inod20,2) * dnxi_27(20,ix)           &
     &                        + xx(inod21,2) * dnxi_27(21,ix)           &
     &                        + xx(inod22,2) * dnxi_27(22,ix)           &
     &                        + xx(inod23,2) * dnxi_27(23,ix)           &
     &                        + xx(inod24,2) * dnxi_27(24,ix)           &
     &                        + xx(inod25,2) * dnxi_27(25,ix)           &
     &                        + xx(inod26,2) * dnxi_27(26,ix)           &
     &                        + xx(inod27,2) * dnxi_27(27,ix)           &
     &                         )  * owe3d(ix)
!
            dydei_ele(iele) = dydei_ele(iele)                           &
     &                      + ( xx(inod1, 2) * dnei_27(1, ix)           &
     &                        + xx(inod2, 2) * dnei_27(2, ix)           &
     &                        + xx(inod3, 2) * dnei_27(3, ix)           &
     &                        + xx(inod4, 2) * dnei_27(4, ix)           &
     &                        + xx(inod5, 2) * dnei_27(5, ix)           &
     &                        + xx(inod6, 2) * dnei_27(6, ix)           &
     &                        + xx(inod7, 2) * dnei_27(7, ix)           &
     &                        + xx(inod8, 2) * dnei_27(8, ix)           &
     &                        + xx(inod9, 2) * dnei_27(9, ix)           &
     &                        + xx(inod10,2) * dnei_27(10,ix)           &
     &                        + xx(inod11,2) * dnei_27(11,ix)           &
     &                        + xx(inod12,2) * dnei_27(12,ix)           &
     &                        + xx(inod13,2) * dnei_27(13,ix)           &
     &                        + xx(inod14,2) * dnei_27(14,ix)           &
     &                        + xx(inod15,2) * dnei_27(15,ix)           &
     &                        + xx(inod16,2) * dnei_27(16,ix)           &
     &                        + xx(inod17,2) * dnei_27(17,ix)           &
     &                        + xx(inod18,2) * dnei_27(18,ix)           &
     &                        + xx(inod19,2) * dnei_27(19,ix)           &
     &                        + xx(inod20,2) * dnei_27(20,ix)           &
     &                        + xx(inod21,2) * dnei_27(21,ix)           &
     &                        + xx(inod22,2) * dnei_27(22,ix)           &
     &                        + xx(inod23,2) * dnei_27(23,ix)           &
     &                        + xx(inod24,2) * dnei_27(24,ix)           &
     &                        + xx(inod25,2) * dnei_27(25,ix)           &
     &                        + xx(inod26,2) * dnei_27(26,ix)           &
     &                        + xx(inod27,2) * dnei_27(27,ix)           &
     &                         )  * owe3d(ix)
!
            dydzi_ele(iele) = dydzi_ele(iele)                             &
     &                      + ( xx(inod1, 2) * dnzi_27(1, ix)           &
     &                        + xx(inod2, 2) * dnzi_27(2, ix)           &
     &                        + xx(inod3, 2) * dnzi_27(3, ix)           &
     &                        + xx(inod4, 2) * dnzi_27(4, ix)           &
     &                        + xx(inod5, 2) * dnzi_27(5, ix)           &
     &                        + xx(inod6, 2) * dnzi_27(6, ix)           &
     &                        + xx(inod7, 2) * dnzi_27(7, ix)           &
     &                        + xx(inod8, 2) * dnzi_27(8, ix)           &
     &                        + xx(inod9, 2) * dnzi_27(9, ix)           &
     &                        + xx(inod10,2) * dnzi_27(10,ix)           &
     &                        + xx(inod11,2) * dnzi_27(11,ix)           &
     &                        + xx(inod12,2) * dnzi_27(12,ix)           &
     &                        + xx(inod13,2) * dnzi_27(13,ix)           &
     &                        + xx(inod14,2) * dnzi_27(14,ix)           &
     &                        + xx(inod15,2) * dnzi_27(15,ix)           &
     &                        + xx(inod16,2) * dnzi_27(16,ix)           &
     &                        + xx(inod17,2) * dnzi_27(17,ix)           &
     &                        + xx(inod18,2) * dnzi_27(18,ix)           &
     &                        + xx(inod19,2) * dnzi_27(19,ix)           &
     &                        + xx(inod20,2) * dnzi_27(20,ix)           &
     &                        + xx(inod21,2) * dnzi_27(21,ix)           &
     &                        + xx(inod22,2) * dnzi_27(22,ix)           &
     &                        + xx(inod23,2) * dnzi_27(23,ix)           &
     &                        + xx(inod24,2) * dnzi_27(24,ix)           &
     &                        + xx(inod25,2) * dnzi_27(25,ix)           &
     &                        + xx(inod26,2) * dnzi_27(26,ix)           &
     &                        + xx(inod27,2) * dnzi_27(27,ix)           &
     &                         )  * owe3d(ix)
!
!
            dzdxi_ele(iele) = dzdxi_ele(iele)                           &
     &                      + ( xx(inod1, 3) * dnxi_27(1, ix)           &
     &                        + xx(inod2, 3) * dnxi_27(2, ix)           &
     &                        + xx(inod3, 3) * dnxi_27(3, ix)           &
     &                        + xx(inod4, 3) * dnxi_27(4, ix)           &
     &                        + xx(inod5, 3) * dnxi_27(5, ix)           &
     &                        + xx(inod6, 3) * dnxi_27(6, ix)           &
     &                        + xx(inod7, 3) * dnxi_27(7, ix)           &
     &                        + xx(inod8, 3) * dnxi_27(8, ix)           &
     &                        + xx(inod9, 3) * dnxi_27(9, ix)           &
     &                        + xx(inod10,3) * dnxi_27(10,ix)           &
     &                        + xx(inod11,3) * dnxi_27(11,ix)           &
     &                        + xx(inod12,3) * dnxi_27(12,ix)           &
     &                        + xx(inod13,3) * dnxi_27(13,ix)           &
     &                        + xx(inod14,3) * dnxi_27(14,ix)           &
     &                        + xx(inod15,3) * dnxi_27(15,ix)           &
     &                        + xx(inod16,3) * dnxi_27(16,ix)           &
     &                        + xx(inod17,3) * dnxi_27(17,ix)           &
     &                        + xx(inod18,3) * dnxi_27(18,ix)           &
     &                        + xx(inod19,3) * dnxi_27(19,ix)           &
     &                        + xx(inod20,3) * dnxi_27(20,ix)           &
     &                        + xx(inod21,3) * dnxi_27(21,ix)           &
     &                        + xx(inod22,3) * dnxi_27(22,ix)           &
     &                        + xx(inod23,3) * dnxi_27(23,ix)           &
     &                        + xx(inod24,3) * dnxi_27(24,ix)           &
     &                        + xx(inod25,3) * dnxi_27(25,ix)           &
     &                        + xx(inod26,3) * dnxi_27(26,ix)           &
     &                        + xx(inod27,3) * dnxi_27(27,ix)           &
     &                         )  * owe3d(ix)
!
            dzdei_ele(iele) = dzdei_ele(iele)                           &
     &                      + ( xx(inod1, 3) * dnei_27(1, ix)           &
     &                        + xx(inod2, 3) * dnei_27(2, ix)           &
     &                        + xx(inod3, 3) * dnei_27(3, ix)           &
     &                        + xx(inod4, 3) * dnei_27(4, ix)           &
     &                        + xx(inod5, 3) * dnei_27(5, ix)           &
     &                        + xx(inod6, 3) * dnei_27(6, ix)           &
     &                        + xx(inod7, 3) * dnei_27(7, ix)           &
     &                        + xx(inod8, 3) * dnei_27(8, ix)           &
     &                        + xx(inod9, 3) * dnei_27(9, ix)           &
     &                        + xx(inod10,3) * dnei_27(10,ix)           &
     &                        + xx(inod11,3) * dnei_27(11,ix)           &
     &                        + xx(inod12,3) * dnei_27(12,ix)           &
     &                        + xx(inod13,3) * dnei_27(13,ix)           &
     &                        + xx(inod14,3) * dnei_27(14,ix)           &
     &                        + xx(inod15,3) * dnei_27(15,ix)           &
     &                        + xx(inod16,3) * dnei_27(16,ix)           &
     &                        + xx(inod17,3) * dnei_27(17,ix)           &
     &                        + xx(inod18,3) * dnei_27(18,ix)           &
     &                        + xx(inod19,3) * dnei_27(19,ix)           &
     &                        + xx(inod20,3) * dnei_27(20,ix)           &
     &                        + xx(inod21,3) * dnei_27(21,ix)           &
     &                        + xx(inod22,3) * dnei_27(22,ix)           &
     &                        + xx(inod23,3) * dnei_27(23,ix)           &
     &                        + xx(inod24,3) * dnei_27(24,ix)           &
     &                        + xx(inod25,3) * dnei_27(25,ix)           &
     &                        + xx(inod26,3) * dnei_27(26,ix)           &
     &                        + xx(inod27,3) * dnei_27(27,ix)           &
     &                         )  * owe3d(ix)
!
            dzdzi_ele(iele) = dzdzi_ele(iele)                           &
     &                      + ( xx(inod1, 3) * dnzi_27(1, ix)           &
     &                        + xx(inod2, 3) * dnzi_27(2, ix)           &
     &                        + xx(inod3, 3) * dnzi_27(3, ix)           &
     &                        + xx(inod4, 3) * dnzi_27(4, ix)           &
     &                        + xx(inod5, 3) * dnzi_27(5, ix)           &
     &                        + xx(inod6, 3) * dnzi_27(6, ix)           &
     &                        + xx(inod7, 3) * dnzi_27(7, ix)           &
     &                        + xx(inod8, 3) * dnzi_27(8, ix)           &
     &                        + xx(inod9, 3) * dnzi_27(9, ix)           &
     &                        + xx(inod10,3) * dnzi_27(10,ix)           &
     &                        + xx(inod11,3) * dnzi_27(11,ix)           &
     &                        + xx(inod12,3) * dnzi_27(12,ix)           &
     &                        + xx(inod13,3) * dnzi_27(13,ix)           &
     &                        + xx(inod14,3) * dnzi_27(14,ix)           &
     &                        + xx(inod15,3) * dnzi_27(15,ix)           &
     &                        + xx(inod16,3) * dnzi_27(16,ix)           &
     &                        + xx(inod17,3) * dnzi_27(17,ix)           &
     &                        + xx(inod18,3) * dnzi_27(18,ix)           &
     &                        + xx(inod19,3) * dnzi_27(19,ix)           &
     &                        + xx(inod20,3) * dnzi_27(20,ix)           &
     &                        + xx(inod21,3) * dnzi_27(21,ix)           &
     &                        + xx(inod22,3) * dnzi_27(22,ix)           &
     &                        + xx(inod23,3) * dnzi_27(23,ix)           &
     &                        + xx(inod24,3) * dnzi_27(24,ix)           &
     &                        + xx(inod25,3) * dnzi_27(25,ix)           &
     &                        + xx(inod26,3) * dnzi_27(26,ix)           &
     &                        + xx(inod27,3) * dnzi_27(27,ix)           &
     &                         )  * owe3d(ix)
!
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine fem_element_length_lag
!
!-----------------------------------------------------------------------
!
      end module int_element_length
