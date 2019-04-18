!
!!      module t_set_new_spectr
!!
!!       subroutine alloc_z_compliment_info(nz_all)
!!       subroutine alloc_work_array_4_r(num)
!!       subroutine alloc_index_4_trans(npl_spec)
!!
!!       subroutine set_new_spectr(nx_all, ny_all, nz_all, npl_spec)
!!
!!       subroutine dealloc_work_array_4_r(npl_spec)
!!       subroutine dealloc_index_4_trans(npl_spec)
!
      module t_set_new_spectr
!
      use m_precision
      use m_constants
!
      implicit none
!
      type new_plane_spectr
        integer(kind=kint ) :: ncomp_nsp
!
        integer(kind=kint ) :: nnod_new_k_org_z
!
        real(kind=kreal), allocatable:: work_array(:,:)
        real(kind=kreal), allocatable:: new_spectr(:,:)
!
        integer(kind=kint), allocatable  ::  idx_field(:)
        integer(kind=kint), allocatable  ::  idx_adams(:)
!
        real   (kind=kreal), allocatable    ::  z_1(:)
        integer(kind=kint ), allocatable    ::  iz_1(:)
      end type new_plane_spectr
!
      private :: set_new_spectr_xsys, set_new_spectr_xsye
      private :: set_new_spectr_xsyl, set_new_spectr_xeys
      private :: set_new_spectr_xeye, set_new_spectr_xeyl
      private :: set_new_spectr_xlys, set_new_spectr_xlye
      private :: set_new_spectr_xlyl
!
!  --------------------------------------------------------------------
!
      contains
!
!  --------------------------------------------------------------------
!
       subroutine alloc_z_compliment_info(nz_all, npl_spec)
!
       integer(kind=kint), intent(in) :: nz_all
       type(new_plane_spectr), intent(inout) :: npl_spec
!
       allocate( npl_spec%z_1(nz_all) )
       allocate( npl_spec%iz_1(nz_all) )
!
       npl_spec%z_1 =  zero
       npl_spec%iz_1 = 0
!
       end subroutine alloc_z_compliment_info
!
!  --------------------------------------------------------------------
!
       subroutine alloc_work_array_4_r(num, npl_spec)
!
       integer(kind = kint), intent(in) :: num
       type(new_plane_spectr), intent(inout) :: npl_spec
!
       integer(kind = kint) :: n1
!
!       write(*,*) 'numnod, ncomp_nsp', numnod, npl_spec%ncomp_nsp
!
       allocate ( npl_spec%new_spectr(num, npl_spec%ncomp_nsp) )
       n1 = npl_spec%nnod_new_k_org_z
       allocate ( npl_spec%work_array(n1,npl_spec%ncomp_nsp) )
!
       npl_spec%new_spectr = zero
       npl_spec%work_array = zero
!
       end subroutine alloc_work_array_4_r
!
!  ---------------------------------------------------------------------
!
       subroutine alloc_index_4_trans(npl_spec)
!
       type(new_plane_spectr), intent(inout) :: npl_spec
!
       allocate ( npl_spec%idx_field(npl_spec%ncomp_nsp) )
       allocate ( npl_spec%idx_adams(npl_spec%ncomp_nsp) )
!
       npl_spec%idx_field = 0
       npl_spec%idx_adams = 0
!
       end subroutine alloc_index_4_trans
!
!  ---------------------------------------------------------------------
!
      subroutine set_new_spectr(nx_all, ny_all, nz_all, npl_spec)
!
      use m_spectr_4_ispack
!
      integer(kind=kint), intent(in) :: nx_all, ny_all, nz_all
      type(new_plane_spectr), intent(inout) :: npl_spec
!
      if (nx_all .lt. kx_max) then
       if (ny_all .lt. ky_max) then
        call set_new_spectr_xsys(nx_all, ny_all, nz_all, npl_spec)
        write(*,*) 'set_new_spectr_xsys finish'
       else if (ny_all .eq. ky_max) then
        call set_new_spectr_xsye(nx_all, ny_all, nz_all, npl_spec)
        write(*,*) 'set_new_spectr_xsye finish'
       else
        call set_new_spectr_xsyl(nx_all, ny_all, nz_all, npl_spec)
        write(*,*) 'set_new_spectr_xsyl finish'
       end if
      else if (nx_all .eq. kx_max) then
       if (ny_all .lt. ky_max) then
        call set_new_spectr_xeys(nx_all, ny_all, nz_all, npl_spec)
        write(*,*) 'set_new_spectr_xeys finish'
       else if (ny_all .eq. ky_max) then
        call set_new_spectr_xeye(nx_all, ny_all, nz_all, npl_spec)
        write(*,*) 'set_new_spectr_xeye finish'
       else
        call set_new_spectr_xeyl(nx_all, ny_all, nz_all, npl_spec)
        write(*,*) 'set_new_spectr_xeyl finish'
       end if
      else
       if (ny_all .lt. ky_max) then
        call set_new_spectr_xlys(nx_all, ny_all, nz_all, npl_spec)
        write(*,*) 'set_new_spectr_xlys finish'
       else if (ny_all .eq. ky_max) then
        call set_new_spectr_xlye(nx_all, ny_all, nz_all, npl_spec)
        write(*,*) 'set_new_spectr_xlye finish'
       else
        call set_new_spectr_xlyl(nx_all, ny_all, nz_all, npl_spec)
        write(*,*) 'set_new_spectr_xlyl finish'
       end if
      end if
!
      end subroutine set_new_spectr
!
!  ---------------------------------------------------------------------
!
      subroutine set_new_spectr_xsys(nx_all, ny_all, nz_all, npl_spec)
!
      use m_spectr_4_ispack
!
      integer(kind=kint), intent(in) :: nx_all, ny_all, nz_all
       type(new_plane_spectr), intent(inout) :: npl_spec
!
      integer(kind=kint) :: i
      integer(kind=kint) :: inod
      integer(kind=kint) :: ix, iy, iz
      integer(kind=kint) :: i1, i3, i5, i7
!
!
      write(*,*) 'total_spectr', num_spectr*num_fft
      write(*,*) 'total_grid', npl_spec%ncomp_nsp, nx_all*ny_all*nz_all
!
      do i = 1, npl_spec%ncomp_nsp
!
        do iz = 1, nz_all
          inod = iz
          i1   = iz
          npl_spec%new_spectr(inod,i) = npl_spec%work_array(i1,i)
        end do
!
        do iz = 1, nz_all
          inod = nz_all + iz
          i1   = nx_all * nz_all + iz
          i3   = i1 + nz_all
          npl_spec%new_spectr(inod,i) = npl_spec%work_array(i1,i)       &
     &                                  - npl_spec%work_array(i3,i)
        end do
!
        do ix = 3, nx_all
         do iz = 1, nz_all
          inod = (ix-1) * nz_all + iz
          i1   = (ix-1) * nz_all + iz
          npl_spec%new_spectr(inod,i) =npl_spec%work_array(i1,i)
         end do
        end do
!
        do iz = 1, nz_all
          inod = (nx_all*nz_all) + iz
          i1   = (ny_all) * (ky_max*nz_all) + iz
          i3   = i1 + (ky_max*nz_all)
          npl_spec%new_spectr(inod,i) = npl_spec%work_array(i1,i)       &
     &                                  - npl_spec%work_array(i3,i)
        end do
!
        do iz = 1, nz_all
          inod = (nx_all*nz_all) + nz_all + iz
          i1   = ny_all * (ky_max*nz_all) + nx_all * nz_all + iz
          i3   = i1 + nz_all
          i5   = i1 + (ky_max*nz_all)
          i7   = i1 + (ky_max*nz_all) + nz_all
          npl_spec%new_spectr(inod,i) =  npl_spec%work_array(i1,i)      &
     &                                  - npl_spec%work_array(i3,i)     &
     &                                  - npl_spec%work_array(i5,i)     &
     &                                  + npl_spec%work_array(i7,i)
        end do
!
        do ix = 3, nx_all
         do iz = 1, nz_all
          inod = (nx_all*nz_all) + (ix-1)*nz_all + iz
          i1   = (ny_all) * (ky_max*nz_all) + (ix-1) * nz_all + iz
          i3   = i1 + (ky_max*nz_all)
          npl_spec%new_spectr(inod,i) = npl_spec%work_array(i1,i)       &
     &                                  - npl_spec%work_array(i3,i)
         end do
        end do
!
       do iy = 3, ny_all
!
        do iz = 1, nz_all
          inod = (iy-1) * (nx_all*nz_all) + iz
          i1   = (iy-1) * (kx_max*nz_all) + iz
          npl_spec%new_spectr(inod,i) = npl_spec%work_array(i1,i)
        end do
!
        do iz = 1, nz_all
          inod = (iy-1) * (nx_all*nz_all) + nz_all + iz
          i1   = (iy-1) * (kx_max*nz_all) + (nx_all) * nz_all + iz
          i3   = i1 + nz_all
          npl_spec%new_spectr(inod,i) = npl_spec%work_array(i1,i)       &
     &                                  - npl_spec%work_array(i3,i)
        end do
!
        do ix = 3, nx_all
         do iz = 1, nz_all
          inod = (iy-1) * (nx_all*nz_all) + (ix-1) * nz_all + iz
          i1   = (iy-1) * (kx_max*nz_all) + (ix-1) * nz_all + iz
          npl_spec%new_spectr(inod,i) = npl_spec%work_array(i1,i)
         end do
        end do
!
       end do
!
      end do
!
      return
      end subroutine set_new_spectr_xsys
!
!  ---------------------------------------------------------------------
!
      subroutine set_new_spectr_xsye(nx_all, ny_all, nz_all, npl_spec)
!
      use m_spectr_4_ispack
!
      integer(kind=kint), intent(in) :: nx_all, ny_all, nz_all
       type(new_plane_spectr), intent(inout) :: npl_spec
!
      integer(kind=kint) :: i
      integer(kind=kint) :: inod
      integer(kind=kint) :: ix, iy, iz
      integer(kind=kint) :: i1, i3
!
!
!
      do i = 1, npl_spec%ncomp_nsp
!
       do iy = 1, ny_all
!
        do iz = 1, nz_all
          inod = (iy-1) * (nx_all*nz_all) + iz
          i1   = (iy-1) * (kx_max*nz_all) + iz
          npl_spec%new_spectr(inod,i) = npl_spec%work_array(i1,i)
        end do
!
        do iz = 1, nz_all
          inod = (iy-1) * (nx_all*nz_all) + nz_all + iz
          i1   = (iy-1) * (kx_max*nz_all) + (nx_all) * nz_all + iz
          i3   = i1 + nz_all
          npl_spec%new_spectr(inod,i) = npl_spec%work_array(i1,i)       &
     &                                  - npl_spec%work_array(i3,i)
        end do
!
        do ix = 3, nx_all
         do iz = 1, nz_all
          inod = (iy-1) * (nx_all*nz_all) + (ix-1) * nz_all + iz
          i1   = (iy-1) * (kx_max*nz_all) + (ix-1) * nz_all + iz
          npl_spec%new_spectr(inod,i) =npl_spec%work_array(i1,i)
         end do
        end do
!
       end do
      end do
!
      end subroutine set_new_spectr_xsye
!
!  ---------------------------------------------------------------------
!
      subroutine set_new_spectr_xsyl(nx_all, ny_all, nz_all, npl_spec)
!
      use m_spectr_4_ispack
!
      integer(kind=kint), intent(in) :: nx_all, ny_all, nz_all
       type(new_plane_spectr), intent(inout) :: npl_spec
!
      integer(kind=kint) :: i
      integer(kind=kint) :: inod
      integer(kind=kint) :: ix, iy, iz
      integer(kind=kint) :: i1, i3
!
!
      do i = 1, npl_spec%ncomp_nsp
!
        do iz = 1, nz_all
          inod = iz
          i1   = iz
          npl_spec%new_spectr(inod,i) =npl_spec%work_array(i1,i)
        end do
!
        do iz = 1, nz_all
          inod = nz_all + iz
          i1   = nx_all * nz_all + iz
          i3   = i1 + nz_all
          npl_spec%new_spectr(inod,i) = npl_spec%work_array(i1,i)       &
     &                                  - npl_spec%work_array(i3,i)
        end do
!
        do ix = 3, nx_all
         do iz = 1, nz_all
          inod = (ix-1) * nz_all + iz
          i1   = (ix-1) * nz_all + iz
          npl_spec%new_spectr(inod,i) = npl_spec%work_array(i1,i)
         end do
        end do
!
        do ix = 1, nx_all
         do iz = 1, nz_all
          inod = (nx_all*nz_all) + (ix-1)*nz_all + iz
          npl_spec%new_spectr(inod,i) = zero
         end do
        end do
!
       do iy = 3, ky_max
!
        do iz = 1, nz_all
          inod = (iy-1) * (nx_all*nz_all) + iz
          i1   = (iy-1) * (kx_max*nz_all) + iz
          npl_spec%new_spectr(inod,i) =npl_spec%work_array(i1,i)
        end do
!
        do iz = 1, nz_all
          inod = (iy-1)*(nx_all*nz_all) + nz_all + iz
          i1   = (iy-1) * (kx_max*nz_all) + (nx_all) * nz_all + iz
          i3   = i1 + nz_all
          npl_spec%new_spectr(inod,i) = npl_spec%work_array(i1,i)       &
     &                                  - npl_spec%work_array(i3,i)
        end do
!
        do ix = 3, nx_all
         do iz = 1, nz_all
          inod = (iy-1) * (nx_all*nz_all) + (ix-1) * nz_all + iz
          i1   = (iy-1) * (kx_max*nz_all) + (ix-1) * nz_all + iz
          npl_spec%new_spectr(inod,i) = npl_spec%work_array(i1,i)
         end do
        end do
!
       end do
!
        do iz = 1, nz_all
          inod = (ky_max) * (nx_all*nz_all) + iz
          i1   =            (kx_max*nz_all) + iz
          npl_spec%new_spectr(inod,i) = npl_spec%work_array(i1,i)
        end do
!
        do iz = 1, nz_all
          inod = (ky_max) * (nx_all*nz_all) + nz_all + iz
          i1   = (kx_max*nz_all) + (nx_all) * nz_all + iz
          i3   = i1 + nz_all
          npl_spec%new_spectr(inod,i) = npl_spec%work_array(i1,i)       &
     &                                  - npl_spec%work_array(i3,i)
        end do
!
        do ix = 3, nx_all
         do iz = 1, nz_all
          inod = (ky_max)*(nx_all*nz_all) + (ix-1)*nz_all + iz
          i1   = (kx_max*nz_all) + (ix-1) * nz_all + iz
          npl_spec%new_spectr(inod,i) = npl_spec%work_array(i1,i)
         end do
        end do
!
       do iy = ky_max+2, ny_all
        do ix = 1, nx_all
         do iz = 1, nz_all
          inod = (iy-1)*(nx_all*nz_all) + (ix-1)*nz_all + iz
          npl_spec%new_spectr(inod,i) = zero
         end do
        end do
       end do
!
      end do
!
!
      end subroutine set_new_spectr_xsyl
!
!  ---------------------------------------------------------------------
!
      subroutine set_new_spectr_xeys(nx_all, ny_all, nz_all, npl_spec)
!
      use m_spectr_4_ispack
!
      integer(kind=kint), intent(in) :: nx_all, ny_all, nz_all
       type(new_plane_spectr), intent(inout) :: npl_spec
!
      integer(kind=kint) :: i
      integer(kind=kint) :: inod
      integer(kind=kint) :: ix, iy, iz
      integer(kind=kint) :: i1, i3
!
!
      do i = 1, npl_spec%ncomp_nsp
!
        do ix = 1, nx_all
         do iz = 1, nz_all
          inod = (ix-1) * nz_all + iz
          i1   = (ix-1) * nz_all + iz
          npl_spec%new_spectr(inod,i) = npl_spec%work_array(i1,i)
         end do
        end do
!
        do ix = 1, nx_all
         do iz = 1, nz_all
          inod = (nx_all*nz_all) + (ix-1)*nz_all + iz
          i1   = (ny_all) * (kx_max*nz_all) + (ix-1) * nz_all + iz
          i3   = i1 + (kx_max*nz_all)
          npl_spec%new_spectr(inod,i) = npl_spec%work_array(i1,i)       &
     &                                  - npl_spec%work_array(i3,i)
         end do
        end do
!
       do iy = 3, ny_all
        do ix = 1, nx_all
!
         do iz = 1, nz_all
          inod = (iy-1) * (nx_all*nz_all) + (ix-1) * nz_all + iz
          i1   = (iy-1) * (kx_max*nz_all) + (ix-1) * nz_all + iz
          npl_spec%new_spectr(inod,i) = npl_spec%work_array(i1,i)
         end do
!
        end do
       end do
!
      end do
!
      return
      end subroutine set_new_spectr_xeys
!
!  ---------------------------------------------------------------------
!
      subroutine set_new_spectr_xeye(nx_all, ny_all, nz_all, npl_spec)
!
      use m_spectr_4_ispack
!
      integer(kind=kint), intent(in) :: nx_all, ny_all, nz_all
       type(new_plane_spectr), intent(inout) :: npl_spec
!
      integer(kind=kint) :: i
      integer(kind=kint) :: inod
      integer(kind=kint) :: ix, iy, iz
      integer(kind=kint) :: i1
!
!
      do i = 1, npl_spec%ncomp_nsp
!
       do iy = 1, ny_all
        do ix = 1, nx_all
!
         do iz = 1, nz_all
          inod = (iy-1) * (nx_all*nz_all) + (ix-1) * nz_all + iz
          i1   = (iy-1) * (kx_max*nz_all) + (ix-1) * nz_all + iz
          npl_spec%new_spectr(inod,i) = npl_spec%work_array(i1,i)
         end do
!
        end do
       end do
!
      end do
!
      return
      end subroutine set_new_spectr_xeye
!
!  ---------------------------------------------------------------------
!
      subroutine set_new_spectr_xeyl(nx_all, ny_all, nz_all, npl_spec)
!
      use m_spectr_4_ispack
!
      integer(kind=kint), intent(in) :: nx_all, ny_all, nz_all
       type(new_plane_spectr), intent(inout) :: npl_spec
!
      integer(kind=kint) :: i
      integer(kind=kint) :: inod
      integer(kind=kint) :: ix, iy, iz
      integer(kind=kint) :: i1
!
!
      do i = 1, npl_spec%ncomp_nsp
!
        do ix = 1, nx_all
         do iz = 1, nz_all
          inod = (ix-1) * nz_all + iz
          i1   = (ix-1) * nz_all + iz
          npl_spec%new_spectr(inod,i) = npl_spec%work_array(i1,i)
         end do
        end do
!
        do ix = 1, nx_all
         do iz = 1, nz_all
          inod = (nx_all*nz_all) + (ix-1)*nz_all + iz
          npl_spec%new_spectr(inod,i) = zero
         end do
        end do
!
       do iy = 3, ky_max
        do ix = 1, nx_all
!
         do iz = 1, nz_all
          inod = (iy-1) * (nx_all*nz_all) + (ix-1) * nz_all + iz
          i1   = (iy-1) * (kx_max*nz_all) + (ix-1) * nz_all + iz
          npl_spec%new_spectr(inod,i) = npl_spec%work_array(i1,i)
         end do
!
        end do
       end do
!
        do ix = 1, nx_all
         do iz = 1, nz_all
          inod = ky_max*(nx_all*nz_all) + (ix-1)*nz_all + iz
          i1   = (kx_max*nz_all) + (ix-1) * nz_all + iz
          npl_spec%new_spectr(inod,i) = npl_spec%work_array(i1,i)
         end do
        end do
!
       do iy = ky_max+2, ny_all
        do ix = 1, nx_all
!
         do iz = 1, nz_all
          inod = (iy-1) * (nx_all*nz_all) + (ix-1)*nz_all + iz
          i1   = (iy-1) * (kx_max*nz_all) + (ix-1) * nz_all + iz
          npl_spec%new_spectr(inod,i) = zero
         end do
!
        end do
       end do
!
      end do
!
      return
      end subroutine set_new_spectr_xeyl
!
!  ---------------------------------------------------------------------
!
      subroutine set_new_spectr_xlys(nx_all, ny_all, nz_all, npl_spec)
!
      use m_spectr_4_ispack
!
      integer(kind=kint), intent(in) :: nx_all, ny_all, nz_all
       type(new_plane_spectr), intent(inout) :: npl_spec
!
      integer(kind=kint) :: i
      integer(kind=kint) :: inod
      integer(kind=kint) :: ix, iy, iz
      integer(kind=kint) :: i1, i3
!
!
      do i = 1, npl_spec%ncomp_nsp
!
        do iz = 1, nz_all
          inod = iz
          i1   = iz
          npl_spec%new_spectr(inod,i) = npl_spec%work_array(i1,i)
        end do
!
        do iz = 1, nz_all
          inod = nz_all + iz
          npl_spec%new_spectr(inod,i) = zero
        end do
!
        do ix = 3, kx_max
         do iz = 1, nz_all
          inod = (ix-1)*nz_all + iz
          i1   = (ix-1) * nz_all + iz
          npl_spec%new_spectr(inod,i) = npl_spec%work_array(i1,i)
         end do
        end do
!
         do iz = 1, nz_all
          inod = kx_max*nz_all + iz
          i1   =  nz_all + iz
          npl_spec%new_spectr(inod,i) = npl_spec%work_array(i1,i)
         end do
!
        do ix = kx_max+2, nx_all
         do iz = 1, nz_all
          inod = (ix-1)*nz_all + iz
          npl_spec%new_spectr(inod,i) = zero
         end do
        end do
!
!
        do iz = 1, nz_all
          inod = (nx_all*nz_all) + iz
          i1   = (ny_all) * (kx_max*nz_all) + iz
          i3   = i1 + (kx_max*nz_all)
          npl_spec%new_spectr(inod,i) = npl_spec%work_array(i1,i)       &
     &                                  - npl_spec%work_array(i3,i)
        end do
!
        do iz = 1, nz_all
          inod = (nx_all*nz_all) + nz_all + iz
          npl_spec%new_spectr(inod,i) = zero
        end do
!
        do ix = 3, kx_max
         do iz = 1, nz_all
          inod = (nx_all*nz_all) + (ix-1)*nz_all + iz
          i1   = (ny_all) * (kx_max*nz_all) + (ix-1) * nz_all + iz
          i3   = i1 + (kx_max*nz_all)
          npl_spec%new_spectr(inod,i) = npl_spec%work_array(i1,i)       &
     &                                 - npl_spec%work_array(i3,i)
         end do
        end do
!
         do iz = 1, nz_all
          inod = (nx_all*nz_all) + kx_max*nz_all + iz
          i1   = (ny_all) * (kx_max*nz_all) + nz_all + iz
          i3   = i1 + (kx_max*nz_all)
          npl_spec%new_spectr(inod,i) = npl_spec%work_array(i1,i)       &
     &                                 - npl_spec%work_array(i3,i)
         end do
!
        do ix = kx_max+2, nx_all
         do iz = 1, nz_all
          inod = (nx_all*nz_all) + (ix-1)*nz_all + iz
          npl_spec%new_spectr(inod,i) = zero
         end do
        end do
!
       do iy = 3, ny_all
!
        do iz = 1, nz_all
          inod = (iy-1) * (nx_all*nz_all) + iz
          i1   = (iy-1) * (kx_max*nz_all) + iz
          npl_spec%new_spectr(inod,i) = npl_spec%work_array(i1,i)
        end do
!
        do iz = 1, nz_all
          inod = (iy-1)*(nx_all*nz_all) + nz_all + iz
          npl_spec%new_spectr(inod,i) = zero
        end do
!
        do ix = 3, kx_max
         do iz = 1, nz_all
          inod = (iy-1) * (nx_all*nz_all) + (ix-1) * nz_all + iz
          i1   = (iy-1) * (kx_max*nz_all) + (ix-1) * nz_all + iz
          npl_spec%new_spectr(inod,i) = npl_spec%work_array(i1,i)
         end do
        end do
!
         do iz = 1, nz_all
          inod = (iy-1) * (nx_all*nz_all) + kx_max*nz_all + iz
          i1   = (iy-1) * (kx_max*nz_all) + nz_all + iz
          npl_spec%new_spectr(inod,i) = npl_spec%work_array(i1,i)
         end do
!
        do ix = kx_max+2, nx_all
         do iz = 1, nz_all
          inod = (iy-1)*(nx_all*nz_all) + (ix-1)*nz_all + iz
          npl_spec%new_spectr(inod,i) = zero
         end do
        end do
       end do
!
      end do
!
      return
      end subroutine set_new_spectr_xlys
!
!
!  ---------------------------------------------------------------------
!
      subroutine set_new_spectr_xlye(nx_all, ny_all, nz_all, npl_spec)
!
      use m_spectr_4_ispack
!
      integer(kind=kint), intent(in) :: nx_all, ny_all, nz_all
       type(new_plane_spectr), intent(inout) :: npl_spec
!
      integer(kind=kint) :: i
      integer(kind=kint) :: inod
      integer(kind=kint) :: ix, iy, iz
      integer(kind=kint) :: i1
!
!
      do i = 1, npl_spec%ncomp_nsp
!
       do iy = 1, ny_all
!
        do iz = 1, nz_all
          inod = (iy-1) * (nx_all*nz_all) + iz
          i1   = (iy-1) * (kx_max*nz_all) + iz
          npl_spec%new_spectr(inod,i) = npl_spec%work_array(i1,i)
        end do
!
        do iz = 1, nz_all
          inod = (iy-1)*(nx_all*nz_all) + nz_all + iz
          npl_spec%new_spectr(inod,i) = zero
        end do
!
        do ix = 3, kx_max
         do iz = 1, nz_all
          inod = (iy-1) * (nx_all*nz_all) + (ix-1)*nz_all + iz
          i1   = (iy-1) * (kx_max*nz_all) + (ix-1) * nz_all + iz
          npl_spec%new_spectr(inod,i) = npl_spec%work_array(i1,i)
         end do
        end do
!
         do iz = 1, nz_all
          inod = (iy-1) * (nx_all*nz_all) + kx_max*nz_all + iz
          i1   = (iy-1) * (kx_max*nz_all) + nz_all + iz
          npl_spec%new_spectr(inod,i) = npl_spec%work_array(i1,i)
         end do
!
        do ix = kx_max+2, nx_all
         do iz = 1, nz_all
          inod = (iy-1) * (nx_all*nz_all) + (ix-1) * nz_all + iz
          npl_spec%new_spectr(inod,i) = zero
         end do
        end do
       end do
!
      end do
!
      return
      end subroutine set_new_spectr_xlye
!
!  ---------------------------------------------------------------------
!
      subroutine set_new_spectr_xlyl(nx_all, ny_all, nz_all, npl_spec)
!
      use m_spectr_4_ispack
!
      integer(kind=kint), intent(in) :: nx_all, ny_all, nz_all
       type(new_plane_spectr), intent(inout) :: npl_spec
!
      integer(kind=kint) :: i
      integer(kind=kint) :: inod
      integer(kind=kint) :: ix, iy, iz
      integer(kind=kint) :: i1
!
!
      do i = 1, npl_spec%ncomp_nsp
!
        do iz = 1, nz_all
          inod = iz
          i1   = iz
          npl_spec%new_spectr(inod,i) = npl_spec%work_array(i1,i)
        end do
!
        do iz = 1, nz_all
          inod = nz_all + iz
          npl_spec%new_spectr(inod,i) = zero
        end do
!
        do ix = 3, kx_max
         do iz = 1, nz_all
          inod = (ix-1) * nz_all + iz
          i1   = (ix-1) * nz_all + iz
          npl_spec%new_spectr(inod,i) = npl_spec%work_array(i1,i)
         end do
        end do
!
         do iz = 1, nz_all
          inod = kx_max*nz_all + iz
          i1   = nz_all + iz
          npl_spec%new_spectr(inod,i) = npl_spec%work_array(i1,i)
         end do
!
        do ix = kx_max+2, nx_all
         do iz = 1, nz_all
          inod = (ix-1)*nz_all + iz
          npl_spec%new_spectr(inod,i) = zero
         end do
        end do
!
        do ix = 1, nx_all
         do iz = 1, nz_all
          inod = (nx_all*nz_all) + (ix-1)*nz_all + iz
          npl_spec%new_spectr(inod,i) = zero
         end do
        end do
!
!
       do iy = 3, ky_max
        do iz = 1, nz_all
          inod = (iy-1) * (nx_all*nz_all) + iz
          i1   = (iy-1) * (kx_max*nz_all) + iz
          npl_spec%new_spectr(inod,i) = npl_spec%work_array(i1,i)
        end do
!
        do iz = 1, nz_all
          inod = (iy-1)*(nx_all*nz_all) + nz_all + iz
          npl_spec%new_spectr(inod,i) = zero
        end do
!
        do ix = 3, kx_max
         do iz = 1, nz_all
          inod = (iy-1) * (nx_all*nz_all) + (ix-1) * nz_all + iz
          i1   = (iy-1) * (kx_max*nz_all) + (ix-1) * nz_all + iz
          npl_spec%new_spectr(inod,i) = npl_spec%work_array(i1,i)
         end do
        end do
!
         do iz = 1, nz_all
          inod = (iy-1) * (nx_all*nz_all) + kx_max*nz_all + iz
          i1   = (iy-1) * (kx_max*nz_all) + nz_all + iz
          npl_spec%new_spectr(inod,i) = npl_spec%work_array(i1,i)
         end do
!
        do ix = kx_max+2, nx_all
         do iz = 1, nz_all
          inod = (iy-1) * (nx_all*nz_all) + (ix-1)*nz_all + iz
          npl_spec%new_spectr(inod,i) = zero
         end do
        end do
       end do
!
        do iz = 1, nz_all
          inod = ky_max*(nx_all*nz_all) + iz
          i1   = (kx_max*nz_all) + iz
          npl_spec%new_spectr(inod,i) = npl_spec%work_array(i1,i)
        end do
!
        do iz = 1, nz_all
          inod = ky_max*(nx_all*nz_all) + nz_all + iz
          npl_spec%new_spectr(inod,i) = zero
        end do
!
        do ix = 3, kx_max
         do iz = 1, nz_all
          inod = ky_max*(nx_all*nz_all) + (ix-1)*nz_all + iz
          i1   = (kx_max*nz_all) + (ix-1) * nz_all + iz
          npl_spec%new_spectr(inod,i) = npl_spec%work_array(i1,i)
         end do
        end do
!
         do iz = 1, nz_all
          inod = ky_max * (nx_all*nz_all) + kx_max*nz_all + iz
          i1   = (kx_max*nz_all) + nz_all + iz
          npl_spec%new_spectr(inod,i) = npl_spec%work_array(i1,i)
         end do
!
        do ix = kx_max+2, nx_all
         do iz = 1, nz_all
          inod = (iy-1)*(nx_all*nz_all) + (ix-1)*nz_all + iz
          npl_spec%new_spectr(inod,i) = zero
         end do
        end do
!
!
       do iy = ky_max+2, ny_all
        do ix = 1, nx_all
         do iz = 1, nz_all
          inod = (iy-1)*(nx_all*nz_all) + (ix-1)*nz_all + iz
          npl_spec%new_spectr(inod,i) = zero
         end do
        end do
       end do
!
      end do
!
      return
      end subroutine set_new_spectr_xlyl
!
!  --------------------------------------------------------------------
!
       subroutine dealloc_work_array_4_r(npl_spec)
!
       type(new_plane_spectr), intent(inout) :: npl_spec
!
       deallocate ( npl_spec%new_spectr )
       deallocate ( npl_spec%work_array )
!
       end subroutine dealloc_work_array_4_r
!
!  ---------------------------------------------------------------------
!
       subroutine dealloc_index_4_trans(npl_spec)
!
       type(new_plane_spectr), intent(inout) :: npl_spec
!
       deallocate ( npl_spec%idx_field )
       deallocate ( npl_spec%idx_adams )
!
       end subroutine dealloc_index_4_trans
!
!  ---------------------------------------------------------------------
!
      end module t_set_new_spectr
