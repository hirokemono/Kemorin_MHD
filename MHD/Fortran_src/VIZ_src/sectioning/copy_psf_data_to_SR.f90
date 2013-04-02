!
!      module copy_psf_data_to_SR
!
!      Written by H. Matsui on July, 2006
!
!      subroutine set_real_data_2_send_psf(ntot_psf, ncomp_dat,         &
!     &          dat_psf, send)
!      subroutine set_int_data_2_send_psf(ntot_psf, ncomp_dat,          &
!     &          ie_psf, isend)
!
!      subroutine set_recv_2_real_data_psf(num_para, max_para,          &
!     &          ncomp_dat, recv, dat_out)
!      subroutine set_recv_2_int_data_psf(num_para, max_para,           &
!     &          ncomp_dat, irecv, ie_out)
!
!      subroutine adjust_patch_connect_4_collect(nprocs, num_psf,       &
!     &          ntot_output, istack_nod_para,                          &
!     &          istack_ele_para, itri, ie_out)
!
      module copy_psf_data_to_SR
!
      use m_precision
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_real_data_2_send_psf(ntot_psf, ncomp_dat,          &
     &          dat_psf, send)
!
      integer(kind = kint), intent(in) :: ntot_psf, ncomp_dat
      real(kind = kreal), intent(in) :: dat_psf(ntot_psf, ncomp_dat)
!
      real(kind = kreal), intent(inout) :: send(ntot_psf*ncomp_dat)
!
      integer(kind = kint) :: inum, nd, k1
!
!
      do nd = 1, ncomp_dat
        do inum = 1, ntot_psf
          k1 = (nd-1)*ntot_psf + inum
          send(k1) = dat_psf(inum,nd)
        end do
      end do
!
      end subroutine set_real_data_2_send_psf
!
! ----------------------------------------------------------------------
!
      subroutine set_int_data_2_send_psf(ntot_psf, ncomp_dat,           &
     &          ie_psf, isend)
!
      integer(kind = kint), intent(in) :: ntot_psf, ncomp_dat
      integer(kind = kint), intent(in) :: ie_psf(ntot_psf, ncomp_dat)
!
      integer(kind = kint), intent(inout) :: isend(ntot_psf*ncomp_dat)
!
      integer(kind = kint) :: inum, nd, k1
!
!
      do nd = 1, ncomp_dat
        do inum = 1, ntot_psf
          k1 = (nd-1)*ntot_psf + inum
          isend(k1) = ie_psf(inum,nd)
        end do
      end do
!
      end subroutine set_int_data_2_send_psf
!
! ----------------------------------------------------------------------
!
      subroutine set_recv_2_real_data_psf(nprocs, num_psf,              &
     &          ntot_output, istack_nod_para, nnod_recv,                &
     &          istack_nod_recv, ncomp_dat, recv, dat_out)
!
      integer(kind = kint), intent(in) :: nprocs, num_psf
      integer(kind = kint), intent(in) :: ntot_output, ncomp_dat
      integer(kind = kint), intent(in)                                  &
     &      :: istack_nod_para(0:nprocs*num_psf)
      integer(kind = kint), intent(in) :: nnod_recv(nprocs*num_psf)
      integer(kind = kint), intent(in)                                  &
     &      :: istack_nod_recv(0:nprocs*num_psf)
      real(kind = kreal), intent(inout) :: recv(ntot_output*ncomp_dat)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: dat_out(ntot_output,ncomp_dat)
!
      integer(kind = kint) :: inum, nd, ip, i, j, k0, kk, jnod, kst
!
!
        do nd = 1, ncomp_dat
          do i = 1, num_psf
            do ip = 1, nprocs
              j = ip + (i-1)*nprocs
              k0 = (ip-1)*num_psf
              kst = ncomp_dat * istack_nod_recv(k0)                     &
     &             + (nd-1) * (istack_nod_recv(num_psf+k0)              &
     &                         -istack_nod_recv( k0 ))                  &
     &              + (istack_nod_recv(k0+i-1) - istack_nod_recv(k0))
              do inum = 1, nnod_recv(k0+i)
                jnod = istack_nod_para(j-1) + inum
                kk = kst + inum
                dat_out(jnod,nd) = recv(kk)
              end do
            end do
          end do
        end do
!
      end subroutine set_recv_2_real_data_psf
!
! ----------------------------------------------------------------------
!
      subroutine set_recv_2_int_data_psf(nprocs, num_psf,               &
     &          ntot_output, istack_ele_para, nele_recv,                &
     &          istack_ele_recv, itri, irecv, ie_out)
!
      integer(kind = kint), intent(in) :: nprocs, num_psf
      integer(kind = kint), intent(in) :: ntot_output, itri
      integer(kind = kint), intent(in)                                  &
     &      :: istack_ele_para(0:nprocs*num_psf)
      integer(kind = kint), intent(in) :: nele_recv(nprocs*num_psf)
      integer(kind = kint), intent(in)                                  &
     &      :: istack_ele_recv(0:nprocs*num_psf)
      integer(kind = kint), intent(in) :: irecv(ntot_output*itri)
!
      integer(kind = kint), intent(inout) :: ie_out(ntot_output,itri)
!
      integer(kind = kint) :: inum, nd, ip, i, j, k0, kk, jele, kst
!
!
      do nd = 1, itri
        do i = 1, num_psf
          do ip = 1, nprocs
            j = ip + (i-1)*nprocs
            k0 = (ip-1)*num_psf
            kst = itri * istack_ele_recv(k0)                            &
     &           + (nd-1) * (istack_ele_recv(num_psf+k0)                &
     &                       -istack_ele_recv( k0 ))                    &
     &            + (istack_ele_recv(k0+i-1) - istack_ele_recv(k0))
            do inum = 1, nele_recv(k0+i)
              jele = istack_ele_para(j-1) + inum
              kk = kst + inum
              ie_out(jele,nd) = irecv(kk)
            end do
          end do
        end do
      end do
!
      end subroutine set_recv_2_int_data_psf
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine adjust_patch_connect_4_collect(nprocs, num_psf,        &
     &          ntot_output, istack_nod_para,                           &
     &          istack_ele_para, itri, ie_out)
!
!
      integer(kind = kint), intent(in) :: nprocs, num_psf
      integer(kind = kint), intent(in) :: ntot_output, itri
      integer(kind = kint), intent(in)                                  &
     &      :: istack_nod_para(0:nprocs*num_psf)
      integer(kind = kint), intent(in)                                  &
     &      :: istack_ele_para(0:nprocs*num_psf)
!
      integer(kind = kint), intent(inout) :: ie_out(ntot_output,itri)
!
      integer(kind = kint) :: ip, nd, i, ist, jst, jed, jele
!
!
!      do nd = 1, itri
!        do i = 1, num_psf
!          ist = (i-1)*nprocs
!          do ip = 1, nprocs
!            jst = istack_ele_para(ip+ist-1) + 1
!            jed = istack_ele_para(ip+ist)
!            do jele = jst, jed
!              ie_out(jele,nd) = ie_out(jele,nd)                         &
!     &                         - istack_nod_para(ist)
!            end do
!          end do
!        end do
!      end do
!
      do nd = 1, itri
        do i = 1, num_psf
          ist = (i-1)*nprocs
          do ip = 1, nprocs
            jst = istack_ele_para(ip+ist-1) + 1
            jed = istack_ele_para(ip+ist)
            do jele = jst, jed
              ie_out(jele,nd) = ie_out(jele,nd)                         &
     &                         + istack_nod_para(ist+ip-1)              &
     &                         - istack_nod_para(ist)
            end do
          end do
        end do
      end do
!
      end subroutine adjust_patch_connect_4_collect
!
! ----------------------------------------------------------------------
!
      end module copy_psf_data_to_SR
