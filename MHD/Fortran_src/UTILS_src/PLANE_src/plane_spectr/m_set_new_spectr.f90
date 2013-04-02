!
      module m_set_new_spectr
!
      use m_precision
      use m_constants
!
      implicit none
!
      integer(kind=kint ) :: ncomp_nsp
!
      integer(kind=kint ) :: nnod_new_k_org_z
!
      real(kind=kreal), dimension(:,:), allocatable:: work_array
      real(kind=kreal), dimension(:,:), allocatable:: new_spectr
!
      integer(kind=kint), dimension(:)  , allocatable  ::  idx_field
      integer(kind=kint), dimension(:)  , allocatable  ::  idx_adams
!
      real   (kind=kreal), dimension(:), allocatable    ::  z_1
      integer(kind=kint ), dimension(:), allocatable    ::  iz_1
!
      private :: set_new_spectr_xsys, set_new_spectr_xsye
      private :: set_new_spectr_xsyl, set_new_spectr_xeys
      private :: set_new_spectr_xeye, set_new_spectr_xeyl
      private :: set_new_spectr_xlys, set_new_spectr_xlye
      private :: set_new_spectr_xlyl
!
!       subroutine allocate_z_compliment_info(nz_all)
!       subroutine allocate_work_array_4_r(num)
!       subroutine allocate_index_4_trans
!
!       subroutine set_new_spectr
!
!       subroutine deallocate_work_array_4_r
!       subroutine deallocate_index_4_trans
!
!  --------------------------------------------------------------------
!
      contains
!
!  --------------------------------------------------------------------
!
       subroutine allocate_z_compliment_info(nz_all)
!
       integer(kind=kint ) :: nz_all
!
       allocate( z_1(nz_all) )
       allocate( iz_1(nz_all) )
!
       z_1 =  zero
       iz_1 = 0
!
       end subroutine allocate_z_compliment_info
!
!  --------------------------------------------------------------------
!
       subroutine allocate_work_array_4_r(num)
!
       integer(kind=kint ) :: num
!
!       write(*,*) 'numnod, ncomp_nsp', numnod, ncomp_nsp
!
       allocate ( new_spectr(num, ncomp_nsp) )
       allocate ( work_array(nnod_new_k_org_z,ncomp_nsp) )
!
       new_spectr = zero
       work_array = zero
!
       end subroutine allocate_work_array_4_r
!
!  ---------------------------------------------------------------------
!
       subroutine allocate_index_4_trans
!
!
       allocate ( idx_field(ncomp_nsp) )
       allocate ( idx_adams(ncomp_nsp) )
!
       idx_field = 0
       idx_adams = 0
!
       end subroutine allocate_index_4_trans
!
!  ---------------------------------------------------------------------
!
      subroutine set_new_spectr
!
      use m_size_4_plane
      use m_spectr_4_ispack
!
      if (nx_all .lt. kx_max) then
       if (ny_all .lt. ky_max) then
        call set_new_spectr_xsys
        write(*,*) 'set_new_spectr_xsys finish'
       else if (ny_all .eq. ky_max) then
        call set_new_spectr_xsye
        write(*,*) 'set_new_spectr_xsye finish'
       else
        call set_new_spectr_xsyl
        write(*,*) 'set_new_spectr_xsyl finish'
       end if
      else if (nx_all .eq. kx_max) then
       if (ny_all .lt. ky_max) then
        call set_new_spectr_xeys
        write(*,*) 'set_new_spectr_xeys finish'
       else if (ny_all .eq. ky_max) then
        call set_new_spectr_xeye
        write(*,*) 'set_new_spectr_xeye finish'
       else
        call set_new_spectr_xeyl
        write(*,*) 'set_new_spectr_xeyl finish'
       end if
      else
       if (ny_all .lt. ky_max) then
        call set_new_spectr_xlys
        write(*,*) 'set_new_spectr_xlys finish'
       else if (ny_all .eq. ky_max) then
        call set_new_spectr_xlye
        write(*,*) 'set_new_spectr_xlye finish'
       else
        call set_new_spectr_xlyl
        write(*,*) 'set_new_spectr_xlyl finish'
       end if
      end if
!
      end subroutine set_new_spectr
!
!  ---------------------------------------------------------------------
!
      subroutine set_new_spectr_xsys
!
      use m_size_4_plane
      use m_spectr_4_ispack
!
      integer(kind=kint) :: i
      integer(kind=kint) :: inod
      integer(kind=kint) :: ix, iy, iz
      integer(kind=kint) :: i1, i3, i5, i7
!
!
      write(*,*) 'total_spectr', num_spectr*num_fft
      write(*,*) 'total_grid', ncomp_nsp, nx_all*ny_all*nz_all
!
      do i = 1, ncomp_nsp
!
        do iz = 1, nz_all
          inod = iz
          i1   = iz
          new_spectr(inod,i) = work_array(i1,i)
        end do
!
        do iz = 1, nz_all
          inod = nz_all + iz
          i1   = nx_all * nz_all + iz
          i3   = i1 + nz_all
          new_spectr(inod,i) = work_array(i1,i) - work_array(i3,i)
        end do
!
        do ix = 3, nx_all
         do iz = 1, nz_all
          inod = (ix-1) * nz_all + iz
          i1   = (ix-1) * nz_all + iz
          new_spectr(inod,i) =work_array(i1,i)
         end do
        end do
!
        do iz = 1, nz_all
          inod = (nx_all*nz_all) + iz
          i1   = (ny_all) * (ky_max*nz_all) + iz
          i3   = i1 + (ky_max*nz_all)
          new_spectr(inod,i) = work_array(i1,i) - work_array(i3,i)
        end do
!
        do iz = 1, nz_all
          inod = (nx_all*nz_all) + nz_all + iz
          i1   = ny_all * (ky_max*nz_all) + nx_all * nz_all + iz
          i3   = i1 + nz_all
          i5   = i1 + (ky_max*nz_all)
          i7   = i1 + (ky_max*nz_all) + nz_all
          new_spectr(inod,i) =  work_array(i1,i) - work_array(i3,i)     &
     &                        - work_array(i5,i) + work_array(i7,i)
        end do
!
        do ix = 3, nx_all
         do iz = 1, nz_all
          inod = (nx_all*nz_all) + (ix-1)*nz_all + iz
          i1   = (ny_all) * (ky_max*nz_all) + (ix-1) * nz_all + iz
          i3   = i1 + (ky_max*nz_all)
          new_spectr(inod,i) = work_array(i1,i) - work_array(i3,i)
         end do
        end do
!
       do iy = 3, ny_all
!
        do iz = 1, nz_all
          inod = (iy-1) * (nx_all*nz_all) + iz
          i1   = (iy-1) * (kx_max*nz_all) + iz
          new_spectr(inod,i) = work_array(i1,i)
        end do
!
        do iz = 1, nz_all
          inod = (iy-1) * (nx_all*nz_all) + nz_all + iz
          i1   = (iy-1) * (kx_max*nz_all) + (nx_all) * nz_all + iz
          i3   = i1 + nz_all
          new_spectr(inod,i) = work_array(i1,i) - work_array(i3,i)
        end do
!
        do ix = 3, nx_all
         do iz = 1, nz_all
          inod = (iy-1) * (nx_all*nz_all) + (ix-1) * nz_all + iz
          i1   = (iy-1) * (kx_max*nz_all) + (ix-1) * nz_all + iz
          new_spectr(inod,i) = work_array(i1,i)
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
      subroutine set_new_spectr_xsye
!
      use m_size_4_plane
      use m_spectr_4_ispack
!
      integer(kind=kint) :: i
      integer(kind=kint) :: inod
      integer(kind=kint) :: ix, iy, iz
      integer(kind=kint) :: i1, i3
!
!
!
      do i = 1, ncomp_nsp
!
       do iy = 1, ny_all
!
        do iz = 1, nz_all
          inod = (iy-1) * (nx_all*nz_all) + iz
          i1   = (iy-1) * (kx_max*nz_all) + iz
          new_spectr(inod,i) = work_array(i1,i)
        end do
!
        do iz = 1, nz_all
          inod = (iy-1) * (nx_all*nz_all) + nz_all + iz
          i1   = (iy-1) * (kx_max*nz_all) + (nx_all) * nz_all + iz
          i3   = i3 + nz_all
          new_spectr(inod,i) = work_array(i1,i) - work_array(i3,i)
        end do
!
        do ix = 3, nx_all
         do iz = 1, nz_all
          inod = (iy-1) * (nx_all*nz_all) + (ix-1) * nz_all + iz
          i1   = (iy-1) * (kx_max*nz_all) + (ix-1) * nz_all + iz
          new_spectr(inod,i) =work_array(i1,i)
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
      subroutine set_new_spectr_xsyl
!
      use m_size_4_plane
      use m_spectr_4_ispack
!
      integer(kind=kint) :: i
      integer(kind=kint) :: inod
      integer(kind=kint) :: ix, iy, iz
      integer(kind=kint) :: i1, i3
!
!
      do i = 1, ncomp_nsp
!
        do iz = 1, nz_all
          inod = iz
          i1   = iz
          new_spectr(inod,i) =work_array(i1,i)
        end do
!
        do iz = 1, nz_all
          inod = nz_all + iz
          i1   = nx_all * nz_all + iz
          i3   = i1 + nz_all
          new_spectr(inod,i) = work_array(i1,i) - work_array(i3,i)
        end do
!
        do ix = 3, nx_all
         do iz = 1, nz_all
          inod = (ix-1) * nz_all + iz
          i1   = (ix-1) * nz_all + iz
          new_spectr(inod,i) = work_array(i1,i)
         end do
        end do
!
        do ix = 1, nx_all
         do iz = 1, nz_all
          inod = (nx_all*nz_all) + (ix-1)*nz_all + iz
          new_spectr(inod,i) = zero
         end do
        end do
!
       do iy = 3, ky_max
!
        do iz = 1, nz_all
          inod = (iy-1) * (nx_all*nz_all) + iz
          i1   = (iy-1) * (kx_max*nz_all) + iz
          new_spectr(inod,i) =work_array(i1,i)
        end do
!
        do iz = 1, nz_all
          inod = (iy-1)*(nx_all*nz_all) + nz_all + iz
          i1   = (iy-1) * (kx_max*nz_all) + (nx_all) * nz_all + iz
          i3   = i1 + nz_all
          new_spectr(inod,i) = work_array(i1,i) - work_array(i3,i)
        end do
!
        do ix = 3, nx_all
         do iz = 1, nz_all
          inod = (iy-1) * (nx_all*nz_all) + (ix-1) * nz_all + iz
          i1   = (iy-1) * (kx_max*nz_all) + (ix-1) * nz_all + iz
          new_spectr(inod,i) = work_array(i1,i)
         end do
        end do
!
       end do
!
        do iz = 1, nz_all
          inod = (ky_max) * (nx_all*nz_all) + iz
          i1   =            (kx_max*nz_all) + iz
          new_spectr(inod,i) = work_array(i1,i)
        end do
!
        do iz = 1, nz_all
          inod = (ky_max) * (nx_all*nz_all) + nz_all + iz
          i1   = (kx_max*nz_all) + (nx_all) * nz_all + iz
          i3   = i1 + nz_all
          new_spectr(inod,i) = work_array(i1,i) - work_array(i3,i)
        end do
!
        do ix = 3, nx_all
         do iz = 1, nz_all
          inod = (ky_max)*(nx_all*nz_all) + (ix-1)*nz_all + iz
          i1   = (kx_max*nz_all) + (ix-1) * nz_all + iz
          new_spectr(inod,i) = work_array(i1,i)
         end do
        end do
!
       do iy = ky_max+2, ny_all
        do ix = 1, nx_all
         do iz = 1, nz_all
          inod = (iy-1)*(nx_all*nz_all) + (ix-1)*nz_all + iz
          new_spectr(inod,i) = zero
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
      subroutine set_new_spectr_xeys
!
      use m_size_4_plane
      use m_spectr_4_ispack
!
      integer(kind=kint) :: i
      integer(kind=kint) :: inod
      integer(kind=kint) :: ix, iy, iz
      integer(kind=kint) :: i1, i3
!
!
      do i = 1, ncomp_nsp
!
        do ix = 1, nx_all
         do iz = 1, nz_all
          inod = (ix-1) * nz_all + iz
          i1   = (ix-1) * nz_all + iz
          new_spectr(inod,i) = work_array(i1,i)
         end do
        end do
!
        do ix = 1, nx_all
         do iz = 1, nz_all
          inod = (nx_all*nz_all) + (ix-1)*nz_all + iz
          i1   = (ny_all) * (kx_max*nz_all) + (ix-1) * nz_all + iz
          i3   = i1 + (kx_max*nz_all)
          new_spectr(inod,i) = work_array(i1,i) - work_array(i3,i)
         end do
        end do
!
       do iy = 3, ny_all
        do ix = 1, nx_all
!
         do iz = 1, nz_all
          inod = (iy-1) * (nx_all*nz_all) + (ix-1) * nz_all + iz
          i1   = (iy-1) * (kx_max*nz_all) + (ix-1) * nz_all + iz
          new_spectr(inod,i) = work_array(i1,i)
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
      subroutine set_new_spectr_xeye
!
      use m_size_4_plane
      use m_spectr_4_ispack
!
      integer(kind=kint) :: i
      integer(kind=kint) :: inod
      integer(kind=kint) :: ix, iy, iz
      integer(kind=kint) :: i1
!
!
      do i = 1, ncomp_nsp
!
       do iy = 1, ny_all
        do ix = 1, nx_all
!
         do iz = 1, nz_all
          inod = (iy-1) * (nx_all*nz_all) + (ix-1) * nz_all + iz
          i1   = (iy-1) * (kx_max*nz_all) + (ix-1) * nz_all + iz
          new_spectr(inod,i) = work_array(i1,i)
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
      subroutine set_new_spectr_xeyl
!
      use m_size_4_plane
      use m_spectr_4_ispack
!
      integer(kind=kint) :: i
      integer(kind=kint) :: inod
      integer(kind=kint) :: ix, iy, iz
      integer(kind=kint) :: i1
!
!
      do i = 1, ncomp_nsp
!
        do ix = 1, nx_all
         do iz = 1, nz_all
          inod = (ix-1) * nz_all + iz
          i1   = (ix-1) * nz_all + iz
          new_spectr(inod,i) = work_array(i1,i)
         end do
        end do
!
        do ix = 1, nx_all
         do iz = 1, nz_all
          inod = (nx_all*nz_all) + (ix-1)*nz_all + iz
          new_spectr(inod,i) = zero
         end do
        end do
!
       do iy = 3, ky_max
        do ix = 1, nx_all
!
         do iz = 1, nz_all
          inod = (iy-1) * (nx_all*nz_all) + (ix-1) * nz_all + iz
          i1   = (iy-1) * (kx_max*nz_all) + (ix-1) * nz_all + iz
          new_spectr(inod,i) = work_array(i1,i)
         end do
!
        end do
       end do
!
        do ix = 1, nx_all
         do iz = 1, nz_all
          inod = ky_max*(nx_all*nz_all) + (ix-1)*nz_all + iz
          i1   = (kx_max*nz_all) + (ix-1) * nz_all + iz
          new_spectr(inod,i) = work_array(i1,i)
         end do
        end do
!
       do iy = ky_max+2, ny_all
        do ix = 1, nx_all
!
         do iz = 1, nz_all
          inod = (iy-1) * (nx_all*nz_all) + (ix-1)*nz_all + iz
          i1   = (iy-1) * (kx_max*nz_all) + (ix-1) * nz_all + iz
          new_spectr(inod,i) = zero
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
      subroutine set_new_spectr_xlys
!
      use m_size_4_plane
      use m_spectr_4_ispack
!
      integer(kind=kint) :: i
      integer(kind=kint) :: inod
      integer(kind=kint) :: ix, iy, iz
      integer(kind=kint) :: i1, i3
!
!
      do i = 1, ncomp_nsp
!
        do iz = 1, nz_all
          inod = iz
          i1   = iz
          new_spectr(inod,i) = work_array(i1,i)
        end do
!
        do iz = 1, nz_all
          inod = nz_all + iz
          new_spectr(inod,i) = zero
        end do
!
        do ix = 3, kx_max
         do iz = 1, nz_all
          inod = (ix-1)*nz_all + iz
          i1   = (ix-1) * nz_all + iz
          new_spectr(inod,i) = work_array(i1,i)
         end do
        end do
!
         do iz = 1, nz_all
          inod = kx_max*nz_all + iz
          i1   =  nz_all + iz
          new_spectr(inod,i) = work_array(i1,i)
         end do
!
        do ix = kx_max+2, nx_all
         do iz = 1, nz_all
          inod = (ix-1)*nz_all + iz
          new_spectr(inod,i) = zero
         end do
        end do
!
!
        do iz = 1, nz_all
          inod = (nx_all*nz_all) + iz
          i1   = (ny_all) * (kx_max*nz_all) + iz
          i3   = i1 + (kx_max*nz_all)
          new_spectr(inod,i) = work_array(i1,i) - work_array(i3,i)
        end do
!
        do iz = 1, nz_all
          inod = (nx_all*nz_all) + nz_all + iz
          new_spectr(inod,i) = zero
        end do
!
        do ix = 3, kx_max
         do iz = 1, nz_all
          inod = (nx_all*nz_all) + (ix-1)*nz_all + iz
          i1   = (ny_all) * (kx_max*nz_all) + (ix-1) * nz_all + iz
          i3   = i1 + (kx_max*nz_all)
          new_spectr(inod,i) = work_array(i1,i) - work_array(i3,i)
         end do
        end do
!
         do iz = 1, nz_all
          inod = (nx_all*nz_all) + kx_max*nz_all + iz
          i1   = (ny_all) * (kx_max*nz_all) + nz_all + iz
          i3   = i1 + (kx_max*nz_all)
          new_spectr(inod,i) = work_array(i1,i) - work_array(i3,i)
         end do
!
        do ix = kx_max+2, nx_all
         do iz = 1, nz_all
          inod = (nx_all*nz_all) + (ix-1)*nz_all + iz
          new_spectr(inod,i) = zero
         end do
        end do
!
       do iy = 3, ny_all
!
        do iz = 1, nz_all
          inod = (iy-1) * (nx_all*nz_all) + iz
          i1   = (iy-1) * (kx_max*nz_all) + iz
          new_spectr(inod,i) = work_array(i1,i)
        end do
!
        do iz = 1, nz_all
          inod = (iy-1)*(nx_all*nz_all) + nz_all + iz
          new_spectr(inod,i) = zero
        end do
!
        do ix = 3, kx_max
         do iz = 1, nz_all
          inod = (iy-1) * (nx_all*nz_all) + (ix-1) * nz_all + iz
          i1   = (iy-1) * (kx_max*nz_all) + (ix-1) * nz_all + iz
          new_spectr(inod,i) = work_array(i1,i)
         end do
        end do
!
         do iz = 1, nz_all
          inod = (iy-1) * (nx_all*nz_all) + kx_max*nz_all + iz
          i1   = (iy-1) * (kx_max*nz_all) + nz_all + iz
          new_spectr(inod,i) = work_array(i1,i)
         end do
!
        do ix = kx_max+2, nx_all
         do iz = 1, nz_all
          inod = (iy-1)*(nx_all*nz_all) + (ix-1)*nz_all + iz
          new_spectr(inod,i) = zero
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
      subroutine set_new_spectr_xlye
!
      use m_size_4_plane
      use m_spectr_4_ispack
!
      integer(kind=kint) :: i
      integer(kind=kint) :: inod
      integer(kind=kint) :: ix, iy, iz
      integer(kind=kint) :: i1
!
!
      do i = 1, ncomp_nsp
!
       do iy = 1, ny_all
!
        do iz = 1, nz_all
          inod = (iy-1) * (nx_all*nz_all) + iz
          i1   = (iy-1) * (kx_max*nz_all) + iz
          new_spectr(inod,i) = work_array(i1,i)
        end do
!
        do iz = 1, nz_all
          inod = (iy-1)*(nx_all*nz_all) + nz_all + iz
          new_spectr(inod,i) = zero
        end do
!
        do ix = 3, kx_max
         do iz = 1, nz_all
          inod = (iy-1) * (nx_all*nz_all) + (ix-1)*nz_all + iz
          i1   = (iy-1) * (kx_max*nz_all) + (ix-1) * nz_all + iz
          new_spectr(inod,i) = work_array(i1,i)
         end do
        end do
!
         do iz = 1, nz_all
          inod = (iy-1) * (nx_all*nz_all) + kx_max*nz_all + iz
          i1   = (iy-1) * (kx_max*nz_all) + nz_all + iz
          new_spectr(inod,i) = work_array(i1,i)
         end do
!
        do ix = kx_max+2, nx_all
         do iz = 1, nz_all
          inod = (iy-1) * (nx_all*nz_all) + (ix-1) * nz_all + iz
          new_spectr(inod,i) = zero
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
      subroutine set_new_spectr_xlyl
!
      use m_size_4_plane
      use m_spectr_4_ispack
!
      integer(kind=kint) :: i
      integer(kind=kint) :: inod
      integer(kind=kint) :: ix, iy, iz
      integer(kind=kint) :: i1
!
!
      do i = 1, ncomp_nsp
!
        do iz = 1, nz_all
          inod = iz
          i1   = iz
          new_spectr(inod,i) = work_array(i1,i)
        end do
!
        do iz = 1, nz_all
          inod = nz_all + iz
          new_spectr(inod,i) = zero
        end do
!
        do ix = 3, kx_max
         do iz = 1, nz_all
          inod = (ix-1) * nz_all + iz
          i1   = (ix-1) * nz_all + iz
          new_spectr(inod,i) = work_array(i1,i)
         end do
        end do
!
         do iz = 1, nz_all
          inod = kx_max*nz_all + iz
          i1   = nz_all + iz
          new_spectr(inod,i) = work_array(i1,i)
         end do
!
        do ix = kx_max+2, nx_all
         do iz = 1, nz_all
          inod = (ix-1)*nz_all + iz
          new_spectr(inod,i) = zero
         end do
        end do
!
        do ix = 1, nx_all
         do iz = 1, nz_all
          inod = (nx_all*nz_all) + (ix-1)*nz_all + iz
          new_spectr(inod,i) = zero
         end do
        end do
!
!
       do iy = 3, ky_max
        do iz = 1, nz_all
          inod = (iy-1) * (nx_all*nz_all) + iz
          i1   = (iy-1) * (kx_max*nz_all) + iz
          new_spectr(inod,i) = work_array(i1,i)
        end do
!
        do iz = 1, nz_all
          inod = (iy-1)*(nx_all*nz_all) + nz_all + iz
          new_spectr(inod,i) = zero
        end do
!
        do ix = 3, kx_max
         do iz = 1, nz_all
          inod = (iy-1) * (nx_all*nz_all) + (ix-1) * nz_all + iz
          i1   = (iy-1) * (kx_max*nz_all) + (ix-1) * nz_all + iz
          new_spectr(inod,i) = work_array(i1,i)
         end do
        end do
!
         do iz = 1, nz_all
          inod = (iy-1) * (nx_all*nz_all) + kx_max*nz_all + iz
          i1   = (iy-1) * (kx_max*nz_all) + nz_all + iz
          new_spectr(inod,i) = work_array(i1,i)
         end do
!
        do ix = kx_max+2, nx_all
         do iz = 1, nz_all
          inod = (iy-1) * (nx_all*nz_all) + (ix-1)*nz_all + iz
          new_spectr(inod,i) = zero
         end do
        end do
       end do
!
        do iz = 1, nz_all
          inod = ky_max*(nx_all*nz_all) + iz
          i1   = (kx_max*nz_all) + iz
          new_spectr(inod,i) = work_array(i1,i)
        end do
!
        do iz = 1, nz_all
          inod = ky_max*(nx_all*nz_all) + nz_all + iz
          new_spectr(inod,i) = zero
        end do
!
        do ix = 3, kx_max
         do iz = 1, nz_all
          inod = ky_max*(nx_all*nz_all) + (ix-1)*nz_all + iz
          i1   = (kx_max*nz_all) + (ix-1) * nz_all + iz
          new_spectr(inod,i) = work_array(i1,i)
         end do
        end do
!
         do iz = 1, nz_all
          inod = ky_max * (nx_all*nz_all) + kx_max*nz_all + iz
          i1   = (kx_max*nz_all) + nz_all + iz
          new_spectr(inod,i) = work_array(i1,i)
         end do
!
        do ix = kx_max+2, nx_all
         do iz = 1, nz_all
          inod = (iy-1)*(nx_all*nz_all) + (ix-1)*nz_all + iz
          new_spectr(inod,i) = zero
         end do
        end do
!
!
       do iy = ky_max+2, ny_all
        do ix = 1, nx_all
         do iz = 1, nz_all
          inod = (iy-1)*(nx_all*nz_all) + (ix-1)*nz_all + iz
          new_spectr(inod,i) = zero
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
       subroutine deallocate_work_array_4_r
!
!
       deallocate ( new_spectr )
       deallocate ( work_array )
!
       end subroutine deallocate_work_array_4_r
!
!  ---------------------------------------------------------------------
!
       subroutine deallocate_index_4_trans
!
!
       deallocate ( idx_field )
       deallocate ( idx_adams )
!
       end subroutine deallocate_index_4_trans
!
!  ---------------------------------------------------------------------
!
      end module m_set_new_spectr
