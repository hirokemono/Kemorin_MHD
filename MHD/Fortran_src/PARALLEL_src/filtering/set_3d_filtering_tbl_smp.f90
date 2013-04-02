!set_3d_filtering_tbl_smp.f90
!      module set_3d_filtering_tbl_smp
!
!     Written by H. Matsui on Nov., 2008
!
!      subroutine count_nnod_3d_filter_smp(np_smp, ngrp_fil,            &
!     &          grp_name_fil, istack_fil, ngrp_fil_smp,                &
!     &          grp_name_fil_smp, num_fil_smp)
!      subroutine set_inod_3d_filter_smp(np_smp, ngrp_fil, istack_fil,  &
!     &          ntot_fil, inod_fil, num_near_fil, ngrp_fil_smp,        &
!     &          num_fil_smp, istack_fil_smp, ntot_fil_smp,             &
!     &          inod_fil_smp, num_near_fil_smp)
!      subroutine set_neib_nod_3d_filter_smp(np_smp, ngrp_fil,          &
!     &          istack_fil, ntot_fil, num_near_fil, istack_near_f,     &
!     &          ntot_near_f, inod_near_f, filter_w, ngrp_fil_smp,      &
!     &          num_fil_smp, istack_fil_smp, ntot_fil_smp,             &
!     &          istack_near_f_smp, ntot_near_f_smp, inod_near_f_smp,   &
!     &          filter_w_smp)
!      subroutine count_num_3d_filtering_sum_smp(np_smp, ngrp_fil_smp,  &
!     &          istack_fil_smp, ntot_fil_smp, num_near_fil_smp,        &
!     &          min_sum_fil_smp, max_sum_fil_smp)
!      subroutine set_start_id_3d_filtering_smp(np_smp, ngrp_fil_smp,   &
!     &          istack_fil_smp, ntot_fil_smp, num_near_fil_smp,        &
!     &          istack_sum_f_smp, ntot_sum_f_smp, ist_sum_f_smp,       &
!     &          ied_sum_f_smp)
!
      module set_3d_filtering_tbl_smp
!
      use m_precision
      use m_constants
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_nnod_3d_filter_smp(np_smp, ngrp_fil,             &
     &          grp_name_fil, istack_fil, ngrp_fil_smp,                 &
     &          grp_name_fil_smp, num_fil_smp)
!
      integer(kind = kint), intent(in) :: np_smp
!
      integer(kind = kint), intent(in) :: ngrp_fil
      character(len=kchara), intent(in) :: grp_name_fil(:)
      integer(kind = kint), intent(in) :: istack_fil(0:ngrp_fil)
!
      integer(kind = kint), intent(in) :: ngrp_fil_smp
!
      character(len=kchara), intent(inout) :: grp_name_fil_smp(:)
      integer(kind = kint), intent(inout)                               &
     &          :: num_fil_smp(ngrp_fil_smp*np_smp)
!
      integer(kind = kint) :: igrp, ist, ied, jst, ip, inum
!
!
      grp_name_fil_smp(1:ngrp_fil_smp) = grp_name_fil(1:ngrp_fil_smp)
!
      do igrp = 1, ngrp_fil_smp
        ist = istack_fil(igrp-1) + 1
        ied = istack_fil(igrp)
        jst = (igrp-1) *np_smp
        ip = 0
        do inum = ist, ied
          ip = ip + 1
          num_fil_smp(jst+ip) = num_fil_smp(jst+ip) + 1
          ip = mod(ip,np_smp)
        end do
      end do
!
      end subroutine count_nnod_3d_filter_smp
!
!  ---------------------------------------------------------------------
!
      subroutine set_inod_3d_filter_smp(np_smp, ngrp_fil, istack_fil,   &
     &          ntot_fil, inod_fil, num_near_fil, ngrp_fil_smp,         &
     &          num_fil_smp, istack_fil_smp, ntot_fil_smp,              &
     &          inod_fil_smp, num_near_fil_smp)
!
      integer(kind = kint), intent(in) :: np_smp
!
      integer(kind = kint), intent(in) :: ngrp_fil
      integer(kind = kint), intent(in) :: istack_fil(0:ngrp_fil)
!
      integer(kind = kint), intent(in) :: ntot_fil
      integer(kind = kint), intent(in) :: inod_fil(ntot_fil)
      integer(kind = kint), intent(in) :: num_near_fil(ntot_fil)
!
      integer(kind = kint), intent(in) :: ngrp_fil_smp
      integer(kind = kint), intent(in)                                  &
     &          :: num_fil_smp(ngrp_fil_smp*np_smp)
      integer(kind = kint), intent(in)                                  &
     &          :: istack_fil_smp(0:ngrp_fil_smp*np_smp)
!
      integer(kind = kint), intent(in) :: ntot_fil_smp
!
      integer(kind = kint), intent(inout) :: inod_fil_smp(ntot_fil_smp)
      integer(kind = kint), intent(inout)                               &
     &          :: num_near_fil_smp(ntot_fil_smp)
!
      integer(kind = kint) :: igrp, ist, jst, ip
      integer(kind = kint) :: kst, inum, jnum, knum
!
!
      do igrp = 1, ngrp_fil_smp
        ist = istack_fil(igrp-1)
        jst = (igrp-1) *np_smp
        do ip = 1, np_smp
          kst = istack_fil_smp(jst+ip-1)
          do inum = 1, num_fil_smp(jst+ip)
            jnum = ist + (inum-1)*np_smp + ip
            knum = kst + inum
            inod_fil_smp(knum) = inod_fil(jnum)
            num_near_fil_smp(knum) = num_near_fil(jnum)
          end do
        end do
      end do
!
      end subroutine set_inod_3d_filter_smp
!
!  ---------------------------------------------------------------------
!
      subroutine set_neib_nod_3d_filter_smp(np_smp, ngrp_fil,           &
     &          istack_fil, ntot_fil, num_near_fil, istack_near_f,      &
     &          ntot_near_f, inod_near_f, filter_w, ngrp_fil_smp,       &
     &          num_fil_smp, istack_fil_smp, ntot_fil_smp,              &
     &          istack_near_f_smp, ntot_near_f_smp, inod_near_f_smp,    &
     &          filter_w_smp)
!
      integer(kind = kint), intent(in) :: np_smp
!
      integer(kind = kint), intent(in) :: ngrp_fil
      integer(kind = kint), intent(in) :: istack_fil(0:ngrp_fil)
!
      integer(kind = kint), intent(in) :: ntot_fil
      integer(kind = kint), intent(in) :: num_near_fil(ntot_fil)
      integer(kind = kint), intent(in) :: istack_near_f(0:ntot_fil)
!
      integer(kind = kint), intent(in) :: ntot_near_f
      integer(kind = kint), intent(in) :: inod_near_f(ntot_near_f)
      real(kind = kreal), intent(in) :: filter_w(ntot_near_f)
!
      integer(kind = kint), intent(in) :: ngrp_fil_smp
      integer(kind = kint), intent(in)                                  &
     &          :: num_fil_smp(ngrp_fil_smp*np_smp)
      integer(kind = kint), intent(in)                                  &
     &          :: istack_fil_smp(0:ngrp_fil_smp*np_smp)
!
      integer(kind = kint), intent(in) :: ntot_fil_smp
      integer(kind = kint), intent(in)                                  &
     &          :: istack_near_f_smp(ntot_fil_smp)
!
      integer(kind = kint), intent(in) :: ntot_near_f_smp
!
      integer(kind = kint), intent(inout)                               &
     &              :: inod_near_f_smp(ntot_near_f_smp)
      real(kind = kreal), intent(inout)                                 &
     &              :: filter_w_smp(ntot_near_f_smp)
!
      integer(kind = kint) :: ip, igrp, ist, jst
      integer(kind = kint) :: kst, inum, jnum, knum, lnum
      integer(kind = kint) :: lst, mst, mnum, nnum
!
!
      do igrp = 1, ngrp_fil_smp
        ist = istack_fil(igrp-1)
        jst = (igrp-1) *np_smp
        do ip = 1, np_smp
          kst = istack_fil_smp(jst+ip-1)
!
          do inum = 1, num_fil_smp(jst+ip)
            jnum = ist + (inum-1)*np_smp + ip
            knum = kst + inum
            lst = istack_near_f(jnum-1)
            mst = istack_near_f_smp(knum-1)
            do lnum = 1, num_near_fil(jnum)
              nnum = lst + lnum
              mnum = mst + lnum
              inod_near_f_smp(mnum) = inod_near_f(nnum)
              filter_w_smp(mnum) = filter_w(nnum)
            end do
          end do
!
        end do
      end do
!
      end subroutine set_neib_nod_3d_filter_smp
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_num_3d_filtering_sum_smp(np_smp, ngrp_fil_smp,   &
     &          istack_fil_smp, ntot_fil_smp, num_near_fil_smp,         &
     &          min_sum_fil_smp, max_sum_fil_smp)
!
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: ngrp_fil_smp
      integer(kind = kint), intent(in)                                  &
     &          :: istack_fil_smp(0:ngrp_fil_smp*np_smp)
!
      integer(kind = kint), intent(in) :: ntot_fil_smp
      integer(kind = kint), intent(in)                                  &
     &          :: num_near_fil_smp(ntot_fil_smp)
!
      integer(kind = kint), intent(inout)                               &
     &          :: min_sum_fil_smp(ngrp_fil_smp*np_smp)
      integer(kind = kint), intent(inout)                               &
     &          :: max_sum_fil_smp(ngrp_fil_smp*np_smp)
!
      integer(kind = kint) :: ip, igrp, jgrp, ist, ied
!
!
      do igrp = 1, ngrp_fil_smp
        do ip = 1, np_smp
          jgrp = (igrp-1)*np_smp + ip
          ist = istack_fil_smp(jgrp-1) + 1
          ied = istack_fil_smp(jgrp)
          min_sum_fil_smp(jgrp) = num_near_fil_smp(ist)
          max_sum_fil_smp(jgrp) = num_near_fil_smp(ied)
        end do
      end do
!
      end subroutine count_num_3d_filtering_sum_smp
!
!  ---------------------------------------------------------------------
!
      subroutine set_start_id_3d_filtering_smp(np_smp, ngrp_fil_smp,    &
     &          istack_fil_smp, ntot_fil_smp, num_near_fil_smp,         &
     &          istack_sum_f_smp, ntot_sum_f_smp, ist_sum_f_smp,        &
     &          ied_sum_f_smp)
!
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: ngrp_fil_smp
      integer(kind = kint), intent(in)                                  &
     &          :: istack_fil_smp(0:ngrp_fil_smp*np_smp)
!
      integer(kind = kint), intent(in) :: ntot_fil_smp
      integer(kind = kint), intent(in)                                  &
     &          :: num_near_fil_smp(ntot_fil_smp)
!
      integer(kind = kint), intent(in)                                  &
     &          :: istack_sum_f_smp(0:ngrp_fil_smp*np_smp)
      integer(kind = kint) :: ntot_sum_f_smp
      integer(kind = kint), intent(inout)                               &
     &          :: ist_sum_f_smp(ntot_sum_f_smp)
      integer(kind = kint), intent(inout)                               &
     &          :: ied_sum_f_smp(ntot_sum_f_smp)
!
      integer(kind = kint) :: ip, igrp, jgrp, ist, ied, inum
      integer(kind = kint) :: jst, jj, jnum
!
!
      do igrp = 1, ngrp_fil_smp
        do ip = 1, np_smp
          jgrp = (igrp-1)*np_smp + ip
          ist = istack_fil_smp(jgrp-1) + 1
          ied = istack_fil_smp(jgrp)
          jst = istack_sum_f_smp(jgrp-1)
          do jnum = 1, num_near_fil_smp(ied)
            jj = jst + jnum
            ied_sum_f_smp(jj) = ied
          end do
!
          do inum = ied, ist+1, -1
            if(num_near_fil_smp(inum)                                   &
     &         .ne. num_near_fil_smp(inum-1) )  then
              do jnum = 1, num_near_fil_smp(inum)
                jj = jst + jnum
                ist_sum_f_smp(jj) = inum
              end do
            end if
          end do
          do jnum = 1, num_near_fil_smp(ist)
            jj = jst + jnum
            ist_sum_f_smp(jj) = ist
          end do
!
        end do
      end do
!
      end subroutine set_start_id_3d_filtering_smp
!
!  ---------------------------------------------------------------------
!
      end module set_3d_filtering_tbl_smp
