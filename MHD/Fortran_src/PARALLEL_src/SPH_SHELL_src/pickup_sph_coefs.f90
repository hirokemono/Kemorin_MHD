!>@file   pickup_sph_coefs.f90
!!@brief      module pickup_sph_coefs
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in  Dec., 2012
!
!> @brief Pick spectr data to output
!!
!!@verbatim
!!      subroutine init_sph_spec_4_monitor(rj_fld)
!!      subroutine pickup_sph_spec_4_monitor(sph_rj, n_point,           &
!!     &          num_phys_rj, ntot_phys_rj, istack_phys_comp_rj, d_rj)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!@endverbatim
!
      module pickup_sph_coefs
!
      use m_precision
      use m_constants
!
      use m_pickup_sph_spectr_data
!
      use t_spheric_rj_data
!
      use pickup_sph_spectr
!
      implicit  none
!
      private :: init_sph_radial_monitor_list
      private :: count_sph_labels_4_monitor, set_sph_fld_id_4_monitor
      private :: set_sph_labels_4_monitor
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_sph_spec_4_monitor(l_truncation, sph_rj, rj_fld)
!
      use calypso_mpi
      use quicksort
!
      use t_phys_data
!
      integer(kind = kint), intent(in) :: l_truncation
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: l
!
!
      call init_sph_radial_monitor_list(sph_rj)
!
      if( (num_pick_sph+num_pick_sph_l+num_pick_sph_m) .eq. 0) return
!
      if(num_pick_sph_l .eq. -9999) then
        num_pick_sph_l = l_truncation+1
        call allocate_pick_sph_l
        do l = 0, l_truncation
          idx_pick_sph_l(l+1) = l
        end do
      end if
!
      call allocate_iflag_pick_sph(l_truncation)
!
      call count_sph_labels_4_monitor(rj_fld%num_phys,                  &
     &    rj_fld%num_component, rj_fld%iflag_monitor)
      call count_picked_sph_adrress(l_truncation,                       &
     &    num_pick_sph, num_pick_sph_l, num_pick_sph_m,                 &
     &    idx_pick_sph_mode, idx_pick_sph_l, idx_pick_sph_m,            &
     &    num_pick_sph_mode)
!
      call allocate_pick_sph_monitor
!
      call set_picked_sph_address(l_truncation, sph_rj,                 &
     &    num_pick_sph, num_pick_sph_l, num_pick_sph_m,                 &
     &    idx_pick_sph_mode, idx_pick_sph_l, idx_pick_sph_m,            &
     &    num_pick_sph_mode, idx_pick_sph_gl, idx_pick_sph_lc)
      call set_scale_4_vect_l0                                          &
     &   (num_pick_sph_mode, idx_pick_sph_gl(1,1), scale_for_zelo(1))
      call deallocate_iflag_pick_sph
      call deallocate_pick_sph_mode
!
      call set_sph_fld_id_4_monitor(rj_fld%num_phys,                    &
     &    rj_fld%num_component, rj_fld%iflag_monitor)
!
      if(my_rank .ne. 0) return
      call set_sph_labels_4_monitor                                     &
     &   (rj_fld%num_phys, rj_fld%num_component, rj_fld%phys_name)
!
      end subroutine init_sph_spec_4_monitor
!
! -----------------------------------------------------------------------
!
      subroutine init_sph_radial_monitor_list(sph_rj)
!
      use calypso_mpi
      use quicksort
!
      type(sph_rj_grid), intent(in) :: sph_rj
!
      integer(kind = kint) :: k, knum
!
!
      if( (num_pick_sph+num_pick_sph_l+num_pick_sph_m) .eq. 0) return
!
      if(pick1%num_layer .le. 0) then
        pick1%num_layer = sph_rj%nidx_rj(1) + sph_rj%iflag_rj_center
!
        call allocate_num_pick_layer
!
        do k = 1, pick1%num_layer
          pick1%id_radius(k) = k - sph_rj%iflag_rj_center
        end do
      end if
      call quicksort_int(pick1%num_layer, pick1%id_radius,              &
     &    ione, pick1%num_layer)
!
!
      do knum = 1, pick1%num_layer
        k = pick1%id_radius(knum)
        if(k .le. 0) then
          pick1%radius_gl(knum) = 0.0d0
        else
          pick1%radius_gl(knum) = sph_rj%radius_1d_rj_r(k)
        end if
      end do
!
      end subroutine init_sph_radial_monitor_list
!
! -----------------------------------------------------------------------
!
      subroutine pickup_sph_spec_4_monitor(sph_rj, n_point,             &
     &          num_phys_rj, ntot_phys_rj, istack_phys_comp_rj, d_rj)
!
      use calypso_mpi
!
      type(sph_rj_grid), intent(in) :: sph_rj
      integer(kind = kint), intent(in) :: n_point
      integer(kind = kint), intent(in) :: num_phys_rj, ntot_phys_rj
      integer(kind = kint), intent(in)                                  &
     &                  :: istack_phys_comp_rj(0:num_phys_rj)
!
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: inum, knum, j, k, nd, i_fld, j_fld
      integer(kind = kint) :: inod, ipick, num, icou, jcou, kst, ncomp
!
!
      if(num_pick_sph_mode * pick1%num_layer .eq. 0) return
!
!$omp parallel do
      do inum = 1, num_pick_sph_mode*pick1%num_layer
        d_rj_pick_sph_lc(1:ntot_comp_pick_sph,inum) = zero
      end do
!$omp end parallel do
!
!   Set field at center
      kst = 1
      if(idx_pick_sph_gl(1,1).eq.0                                      &
     &   .and. sph_rj%iflag_rj_center.gt.0) then
        kst = kst + 1
        inod = sph_rj%inod_rj_center
!
        do j_fld = 1, num_fld_pick_sph
          i_fld = ifield_monitor_rj(j_fld)
          ncomp = istack_phys_comp_rj(i_fld)                            &
     &           - istack_phys_comp_rj(i_fld-1)
          icou = istack_phys_comp_rj(i_fld-1)
          jcou = istack_comp_pick_sph(j_fld-1)
          if(ncomp .eq. 3) then
             d_rj_pick_sph_lc(jcou+1,1) = 0.0d0
             d_rj_pick_sph_lc(jcou+2,1) = 0.0d0
             d_rj_pick_sph_lc(jcou+3,1) = 0.0d0
           else
             do nd = 1, ncomp
               d_rj_pick_sph_lc(jcou+nd,1)= d_rj(inod,icou+nd)
             end do
           end if
         end do
!
      end if
!
!!$omp parallel private(j)
      do inum = 1, num_pick_sph_mode
        j = idx_pick_sph_lc(inum)
        if(j .gt. izero) then
!!$omp do private(knum,k,inod,ipick,j_fld,i_fld,icou,jcou,nd)
          do knum = kst, pick1%num_layer
            k = pick1%id_radius(knum)
            inod =  j +    (k-1) * sph_rj%nidx_rj(2)
            ipick = knum + (inum-1) * pick1%num_layer
!
            do j_fld = 1, num_fld_pick_sph
              i_fld = ifield_monitor_rj(j_fld)
              ncomp = istack_phys_comp_rj(i_fld)                        &
     &               - istack_phys_comp_rj(i_fld-1)
              icou = istack_phys_comp_rj(i_fld-1)
              jcou = istack_comp_pick_sph(j_fld-1)
              if(ncomp .eq. 3) then
                  d_rj_pick_sph_lc(jcou+1,ipick)                        &
     &                    = scale_for_zelo(inum) * d_rj(inod,icou+1)
                  d_rj_pick_sph_lc(jcou+2,ipick)                        &
     &                    = scale_for_zelo(inum) * d_rj(inod,icou+3)
                  d_rj_pick_sph_lc(jcou+3,ipick)                        &
     &                    = scale_for_zelo(inum) * d_rj(inod,icou+2)
              else
                do nd = 1, ncomp
                  d_rj_pick_sph_lc(jcou+nd,ipick)= d_rj(inod,icou+nd)
                end do
              end if
            end do
!
          end do
!!$omp end do nowait
        end if
      end do
!!$omp end parallel
!
      num = ntot_comp_pick_sph * pick1%num_layer                        &
     &     * num_pick_sph_mode
      call MPI_allREDUCE(d_rj_pick_sph_lc(1,1), d_rj_pick_sph_gl(1,1),  &
     &    num, CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      end subroutine pickup_sph_spec_4_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine count_sph_labels_4_monitor                             &
     &         (num_phys_rj, num_phys_comp_rj, iflag_monitor_rj)
!
      use m_phys_labels
!
      integer(kind = kint), intent(in) :: num_phys_rj
      integer(kind = kint), intent(in) :: num_phys_comp_rj(num_phys_rj)
      integer(kind = kint), intent(in) :: iflag_monitor_rj(num_phys_rj)
!
      integer(kind = kint) :: i_fld
!
!
      num_fld_pick_sph = 0
      ntot_comp_pick_sph =   0
      do i_fld = 1, num_phys_rj
        if(iflag_monitor_rj(i_fld) .gt. 0) then
          num_fld_pick_sph = num_fld_pick_sph + 1
          ntot_comp_pick_sph = ntot_comp_pick_sph                       &
     &                        + num_phys_comp_rj(i_fld)
        end if
      end do
!
      end subroutine count_sph_labels_4_monitor
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_fld_id_4_monitor                               &
     &         (num_phys_rj, num_phys_comp_rj, iflag_monitor_rj)
!
      integer(kind = kint), intent(in) :: num_phys_rj
      integer(kind = kint), intent(in) :: num_phys_comp_rj(num_phys_rj)
      integer(kind = kint), intent(in) :: iflag_monitor_rj(num_phys_rj)
!
      integer(kind = kint) :: i_fld, j_fld
!
!
      j_fld = 0
      istack_comp_pick_sph(0) = 0
      do i_fld = 1, num_phys_rj
        if(iflag_monitor_rj(i_fld) .gt. 0) then
          j_fld = j_fld + 1
          istack_comp_pick_sph(j_fld) = istack_comp_pick_sph(j_fld-1)   &
     &                                 + num_phys_comp_rj(i_fld)
          ifield_monitor_rj(j_fld) = i_fld
        end if
      end do
!
      end subroutine set_sph_fld_id_4_monitor
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_labels_4_monitor                               &
     &         (num_phys_rj, num_phys_comp_rj, phys_name_rj)
!
      use add_direction_labels
!
      integer(kind = kint), intent(in) :: num_phys_rj
      integer(kind = kint), intent(in) :: num_phys_comp_rj(num_phys_rj)
      character(len=kchara), intent(in) :: phys_name_rj(num_phys_rj)
!
      integer(kind = kint) :: i_fld, j_fld, jcou
!
!
      istack_comp_pick_sph(0) = 0
      do j_fld = 1, num_fld_pick_sph
        i_fld = ifield_monitor_rj(j_fld)
        jcou = istack_comp_pick_sph(j_fld-1)
        if(num_phys_comp_rj(i_fld) .eq. 1) then
          write(pick_sph_spec_name(jcou+1),'(a)')                       &
     &                      trim(phys_name_rj(i_fld))
        else if(num_phys_comp_rj(i_fld) .eq. 3) then
          call add_vector_sph_spectr_label(phys_name_rj(i_fld),         &
     &          pick_sph_spec_name(jcou+1), pick_sph_spec_name(jcou+2), &
     &          pick_sph_spec_name(jcou+3))
        else if(num_phys_comp_rj(i_fld) .eq. 6) then
          call add_tensor_direction_label_rtp(phys_name_rj(i_fld),      &
     &          pick_sph_spec_name(jcou+1), pick_sph_spec_name(jcou+2), &
     &          pick_sph_spec_name(jcou+3), pick_sph_spec_name(jcou+4), &
     &          pick_sph_spec_name(jcou+5), pick_sph_spec_name(jcou+6))
        end if
      end do
!
      end subroutine set_sph_labels_4_monitor
!
! -----------------------------------------------------------------------
!
      end module pickup_sph_coefs
