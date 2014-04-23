!set_data_4_quad27.f90
!      module set_data_4_quad27
!
!      Written by H. Matsui on May, 2006
!
!      subroutine copy_original_data(numnod, num_tot_nod_phys,          &
!     &          d_nod, nnod_27, dat27)
!      subroutine set_fields_on_surf(numnod, numsurf, numele,           &
!     &          ie, ie_surf, num_tot_nod_phys, nnod_27, dat27)
!
      module set_data_4_quad27
!
      use m_precision
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine copy_original_data(numnod, num_tot_nod_phys,           &
     &          d_nod, nnod_27, dat27)
!
      integer(kind = kint), intent(in) :: numnod, nnod_27
      integer(kind = kint), intent(in) :: num_tot_nod_phys
      real(kind=kreal), intent(in) :: d_nod(numnod,num_tot_nod_phys)
!
      real(kind=kreal), intent(inout)                                   &
     &             :: dat27(nnod_27,num_tot_nod_phys)
!
      integer(kind = kint) :: inod
!
!
!$omp parallel do
        do inod = 1, numnod
            dat27(inod,1:num_tot_nod_phys)                              &
     &             = d_nod(inod,1:num_tot_nod_phys)
        end do
!$omp end parallel do
!
      end subroutine copy_original_data
!
!  ---------------------------------------------------------------------
!
      subroutine set_fields_on_surf(numnod, numsurf, numele,            &
     &          ie, ie_surf, num_tot_nod_phys, nnod_27, dat27)
!
      integer(kind = kint), intent(in) :: numnod, numsurf, numele
      integer(kind = kint), intent(in) :: num_tot_nod_phys
      integer(kind = kint), intent(in) :: nnod_27
      integer(kind = kint), intent(in) :: ie(numele,20)
      integer(kind = kint), intent(in) :: ie_surf(numsurf,8)
!
      real(kind=kreal), intent(inout)                                   &
     &             :: dat27(nnod_27,num_tot_nod_phys)
!
      integer(kind = kint) :: inod, iele, isurf, nd
      integer(kind = kint) ::  i1,  i2,  i3,  i4,  i5,  i6,  i7,  i8
      integer(kind = kint) ::  i9, i10, i11, i12, i13, i14, i15, i16
      integer(kind = kint) :: i17, i18, i19, i20
      real(kind = kreal), parameter :: one = 1.0d0
      real(kind = kreal), parameter :: two = 2.0d0, four = 4.0d0
      real(kind = kreal), parameter :: half = one/two, quata = one/four
!
!
!
!$omp parallel do private(nd,inod,i1,i2,i3,i4,i5,i6,i7,i8)
        do isurf = 1, numsurf
          do nd = 1, num_tot_nod_phys
            inod = numnod +isurf
            i1 = ie_surf(isurf,1)
            i2 = ie_surf(isurf,2)
            i3 = ie_surf(isurf,3)
            i4 = ie_surf(isurf,4)
            i5 = ie_surf(isurf,5)
            i6 = ie_surf(isurf,6)
            i7 = ie_surf(isurf,7)
            i8 = ie_surf(isurf,8)
!
            dat27(inod,nd) = - quata * (dat27(i1,nd) + dat27(i2,nd)     &
     &                                + dat27(i3,nd) + dat27(i4,nd) )   &
     &                        + half * (dat27(i5,nd) + dat27(i6,nd)     &
     &                                + dat27(i7,nd) + dat27(i8,nd) )
          end do
        end do
!$omp end parallel do
!
!$omp parallel do private(nd,inod,i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,       &
!$omp&                    i11,i12,i13,i14,i15,i16,i17,i18,i19,i20)
        do iele = 1, numele
          do nd = 1, num_tot_nod_phys
            inod = numnod + numsurf + iele
            i1 =  ie(iele, 1)
            i2 =  ie(iele, 2)
            i3 =  ie(iele, 3)
            i4 =  ie(iele, 4)
            i5 =  ie(iele, 5)
            i6 =  ie(iele, 6)
            i7 =  ie(iele, 7)
            i8 =  ie(iele, 8)
            i9 =  ie(iele, 9)
            i10 = ie(iele,10)
            i11 = ie(iele,11)
            i12 = ie(iele,12)
            i13 = ie(iele,13)
            i14 = ie(iele,14)
            i15 = ie(iele,15)
            i16 = ie(iele,16)
            i17 = ie(iele,17)
            i18 = ie(iele,18)
            i19 = ie(iele,19)
            i20 = ie(iele,20)
!
            dat27(inod,nd) = - quata * (dat27( i1,nd) + dat27( i2,nd)   &
     &                                + dat27( i3,nd) + dat27( i4,nd)   &
     &                                + dat27( i5,nd) + dat27( i6,nd)   &
     &                                + dat27( i7,nd) + dat27( i8,nd) ) &
     &                       + quata * (dat27( i9,nd) + dat27(i10,nd)   &
     &                                + dat27(i11,nd) + dat27(i12,nd)   &
     &                                + dat27(i13,nd) + dat27(i14,nd)   &
     &                                + dat27(i15,nd) + dat27(i16,nd)   &
     &                                + dat27(i17,nd) + dat27(i18,nd)   &
     &                                + dat27(i19,nd) + dat27(i20,nd) )
!
          end do
        end do
!$omp end parallel do
!
      end subroutine set_fields_on_surf
!
!  ---------------------------------------------------------------------
!
      end module set_data_4_quad27
