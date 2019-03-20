!filter_moments_IO_b.f90
!     module filter_moments_IO_b
!
!     Written by H. Matsui
!     modified by H. Matsui on Nov., 2006
!     modified by H. Matsui on Mar., 2008
!
!!      subroutine write_elens_nod_b(nnod,                              &
!!     &          e_x2_nod, e_y2_nod, e_z2_nod,                         &
!!     &          e_xy_nod, e_yz_nod, e_zx_nod,                         &
!!     &          e_x2_nod_dx, e_y2_nod_dx, e_z2_nod_dx,                &
!!     &          e_xy_nod_dx, e_yz_nod_dx, e_zx_nod_dx, bflag)
!!
!!      subroutine read_elength_b(bflag, num, el1, el2, el3)
!!      subroutine read_mom_coefs_dx_b(bflag, num, el1, el2, el3)
!!
!!      subroutine write_elength_b(num, el1, el2, el3, bflag)
!!      subroutine write_mom_coefs_dx_b(num, el1, el2, el3, bflag)
!!        type(binary_IO_flags), intent(inout) :: bflag
!
      module filter_moments_IO_b
!
      use m_precision
      use m_constants
      use binary_IO
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine write_elens_nod_b(nnod,                                &
     &          e_x2_nod, e_y2_nod, e_z2_nod,                           &
     &          e_xy_nod, e_yz_nod, e_zx_nod,                           &
     &          e_x2_nod_dx, e_y2_nod_dx, e_z2_nod_dx,                  &
     &          e_xy_nod_dx, e_yz_nod_dx, e_zx_nod_dx, bflag)
!
      integer(kind = kint_gl), intent(in) :: nnod
      real(kind = kreal), intent(in) :: e_x2_nod(nnod)
      real(kind = kreal), intent(in) :: e_y2_nod(nnod)
      real(kind = kreal), intent(in) :: e_z2_nod(nnod)
!
      real(kind = kreal), intent(in) :: e_xy_nod(nnod)
      real(kind = kreal), intent(in) :: e_yz_nod(nnod)
      real(kind = kreal), intent(in) :: e_zx_nod(nnod)
!
!
      real(kind = kreal), intent(in) :: e_x2_nod_dx(nnod,3)
      real(kind = kreal), intent(in) :: e_y2_nod_dx(nnod,3)
      real(kind = kreal), intent(in) :: e_z2_nod_dx(nnod,3)
!
      real(kind = kreal), intent(in) :: e_xy_nod_dx(nnod,3)
      real(kind = kreal), intent(in) :: e_yz_nod_dx(nnod,3)
      real(kind = kreal), intent(in) :: e_zx_nod_dx(nnod,3)
!
      type(binary_IO_flags), intent(inout) :: bflag
!
!    output coefs for filters for each node
!
!      write(id_file,'(a)') '! dx^2 for each node'
!      write(id_file,'(a)') '! node ID, length of x, y, z'
      call write_elength_b(nnod, e_x2_nod, e_y2_nod, e_z2_nod, bflag)
      if(bflag%ierr_IO .ne. 0) return
!      write(id_file,'(a)') '! dxdy for each node'
!      write(id_file,'(a)') '! node ID, length of x, y, z'
      call write_elength_b(nnod, e_xy_nod, e_yz_nod, e_zx_nod, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
!      write(id_file,'(a)') '! 1st derivative of dx^2 for each node'
!      write(id_file,'(a)')                                             &
!           '! direction of diffrenciate, node ID, length of x, y, z'
      call write_mom_coefs_dx_b                                         &
     &   (nnod, e_x2_nod_dx, e_y2_nod_dx, e_z2_nod_dx, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
!      write(id_file,'(a)') '! 1st derivative of dxdy for each node'
!      write(id_file,'(a)')                                             &
!           '! direction of diffrenciate, node ID, length of x, y, z'
      call write_mom_coefs_dx_b                                         &
     &   (nnod, e_xy_nod_dx, e_yz_nod_dx, e_zx_nod_dx, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine write_elens_nod_b
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_elength_b(bflag, num, el1, el2, el3)
!
      type(binary_IO_flags), intent(inout) :: bflag
      integer(kind = kint_gl), intent(in) :: num
      real(kind = kreal), intent(inout) :: el1(num), el2(num), el3(num)
!
!
      call read_1d_vector_b(bflag, num, el1)
      if(bflag%ierr_IO .ne. 0) return
      call read_1d_vector_b(bflag, num, el2)
      if(bflag%ierr_IO .ne. 0) return
      call read_1d_vector_b(bflag, num, el3)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine read_elength_b
!
! ----------------------------------------------------------------------
!
      subroutine read_mom_coefs_dx_b(bflag, num, el1, el2, el3)
!
      integer(kind = kint_gl), intent(in) :: num
      real(kind = kreal), intent(inout) :: el1(num,3), el2(num,3)
      real(kind = kreal), intent(inout) :: el3(num,3)
      type(binary_IO_flags), intent(inout) :: bflag
!
!
      call read_2d_vector_b(bflag, num, ithree, el1)
      if(bflag%ierr_IO .ne. 0) return
      call read_2d_vector_b(bflag, num, ithree, el2)
      if(bflag%ierr_IO .ne. 0) return
      call read_2d_vector_b(bflag, num, ithree, el3)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine read_mom_coefs_dx_b
!
! ----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_elength_b(num, el1, el2, el3, bflag)
!
      integer(kind = kint_gl), intent(in) :: num
      real(kind = kreal), intent(in) :: el1(num), el2(num), el3(num)
      type(binary_IO_flags), intent(inout) :: bflag
!
!
      call write_1d_vector_b(num, el1, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call write_1d_vector_b(num, el2, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call write_1d_vector_b(num, el3, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine write_elength_b
!
! ----------------------------------------------------------------------
!
      subroutine write_mom_coefs_dx_b(num, el1, el2, el3, bflag)
!
      integer(kind = kint_gl), intent(in) :: num
      real(kind = kreal), intent(in) :: el1(num,3), el2(num,3)
      real(kind = kreal), intent(in) :: el3(num,3)
      type(binary_IO_flags), intent(inout) :: bflag
!
!
      call write_2d_vector_b(num, ithree, el1, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call write_2d_vector_b(num, ithree, el2, bflag)
      if(bflag%ierr_IO .ne. 0) return
      call write_2d_vector_b(num, ithree, el3, bflag)
      if(bflag%ierr_IO .ne. 0) return
!
      end subroutine write_mom_coefs_dx_b
!
! ----------------------------------------------------------------------
!
      end module filter_moments_IO_b
