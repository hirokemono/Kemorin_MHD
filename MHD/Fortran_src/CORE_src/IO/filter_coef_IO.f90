!>@file   filter_coef_IO.f90
!!@brief  module filter_coef_IO
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2006
!!@date Modified in Apr., 2008
!!@date Modified in Nov., 2008
!
!> @brief Data IO routine for ASCII filter data
!!
!!@verbatim
!!      subroutine read_3d_filter_stack(id_file)
!!      subroutine read_3d_filter_weights_coef(id_file)
!!      subroutine read_3d_filter_stack_b(id_file)
!!      subroutine read_3d_filter_weights_coef_b(id_file)
!!
!!      subroutine write_3d_filter_stack(id_file)
!!      subroutine write_3d_filter_weights_coef(id_file)
!!      subroutine write_3d_filter_stack_b(id_file)
!!      subroutine write_3d_filter_weights_coef_b(id_file)
!!@endverbatim
!
      module filter_coef_IO
!
      use m_precision
!
      use m_combained_filter_IO
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_3d_filter_stack(id_file)
!
      use skip_comment_f
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint) :: i, j, ist, ied
      character(len=255) :: character_4_read
!
!
      call skip_comment(character_4_read,id_file)
      read(character_4_read,*) ngrp_nod_filter_IO
!
      call allocate_num_filtering_IO
!
      read(id_file,*) istack_nod_filter_IO(1:ngrp_nod_filter_IO)
!
      call s_cal_numbers_from_stack(ngrp_nod_filter_IO,                 &
     &    num_nod_filter_IO, istack_nod_filter_IO)
      ntot_nod_filter_IO = istack_nod_filter_IO(ngrp_nod_filter_IO)
!
      call allocate_inod_filter_comb_IO
!
      do i = 1, ngrp_nod_filter_IO
        ist = istack_nod_filter_IO(i-1)+1
        ied = istack_nod_filter_IO(i)
        read(id_file,*) grp_name_filter_IO(i)
        do j = ist, ied
          read(id_file,*) inod_filter_IO(j),                            &
     &                    istack_near_nod_filter_IO(j)
        end do
      end do
!
      call s_cal_numbers_from_stack(ntot_nod_filter_IO,                 &
     &    num_near_nod_filter_IO, istack_near_nod_filter_IO)
      ntot_near_nod_filter_IO                                           &
     &       = istack_near_nod_filter_IO(ntot_nod_filter_IO)
!
      end subroutine read_3d_filter_stack
!
!  ---------------------------------------------------------------------
!
      subroutine read_3d_filter_weights_coef(id_file)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint) :: j, itmp
      character(len=255) :: character_4_read
!
!
      call allocate_3d_filter_data_IO
!
      call skip_comment(character_4_read,id_file)
      read(character_4_read,*) itmp
!
      do j = 1, ntot_near_nod_filter_IO
        read(id_file,*) itmp, inod_near_nod_IO(j), filter_func_IO(j),   &
     &                  filter_weight_IO(j)
      end do
!
      end subroutine read_3d_filter_weights_coef
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_3d_filter_stack(id_file)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint) :: i, j, ist, ied
!
!
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '! nodes for filtering'
      write(id_file,'(a)') '!'
!
      write(id_file,'(i12)') ngrp_nod_filter_IO
      write(id_file,'(10i12)')                                          &
     &              istack_nod_filter_IO(1:ngrp_nod_filter_IO)
!
      do i = 1, ngrp_nod_filter_IO
        ist = istack_nod_filter_IO(i-1)+1
        ied = istack_nod_filter_IO(i)
        write(id_file,'(a)') trim(grp_name_filter_IO(i))
        do j = ist, ied
          write(id_file,'(3i12)') inod_filter_IO(j),                    &
     &                            istack_near_nod_filter_IO(j)
        end do
      end do
!
      end subroutine write_3d_filter_stack
!
!  ---------------------------------------------------------------------
!
      subroutine write_3d_filter_weights_coef(id_file)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint) :: j
!
!
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '!   filter coefficients'
      write(id_file,'(a)') '!'
      write(id_file,'(i12)') ntot_near_nod_filter_IO
      do j = 1, ntot_near_nod_filter_IO
        write(id_file,'(2i12,1p2E25.15e3)') j, inod_near_nod_IO(j),     &
     &     filter_func_IO(j), filter_weight_IO(j)
      end do
!
      end subroutine write_3d_filter_weights_coef
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_3d_filter_stack_b(id_file)
!
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: id_file
!
!
      read(id_file) ngrp_nod_filter_IO
!
      call allocate_num_filtering_IO
!
      read(id_file) istack_nod_filter_IO(1:ngrp_nod_filter_IO)
      read(id_file) grp_name_filter_IO(1:ngrp_nod_filter_IO)
!
      call s_cal_numbers_from_stack(ngrp_nod_filter_IO,                 &
     &    num_nod_filter_IO, istack_nod_filter_IO)
      ntot_nod_filter_IO = istack_nod_filter_IO(ngrp_nod_filter_IO)
!
      call allocate_inod_filter_comb_IO
!
      read(id_file) inod_filter_IO(1:ntot_nod_filter_IO)
      read(id_file) istack_near_nod_filter_IO(1:ntot_nod_filter_IO)
!
      call s_cal_numbers_from_stack(ntot_nod_filter_IO,                 &
     &    num_near_nod_filter_IO, istack_near_nod_filter_IO)
      ntot_near_nod_filter_IO                                           &
     &       = istack_near_nod_filter_IO(ntot_nod_filter_IO)
!
      end subroutine read_3d_filter_stack_b
!
!  ---------------------------------------------------------------------
!
      subroutine read_3d_filter_weights_coef_b(id_file)
!
      integer(kind = kint), intent(in) :: id_file
!
!
      call allocate_3d_filter_data_IO
!
      read(id_file) inod_near_nod_IO(1:ntot_near_nod_filter_IO)
      read(id_file) filter_func_IO(1:ntot_near_nod_filter_IO)
      read(id_file) filter_weight_IO(1:ntot_near_nod_filter_IO)
!
      end subroutine read_3d_filter_weights_coef_b
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_3d_filter_stack_b(id_file)
!
      integer(kind = kint), intent(in) :: id_file
!
!
      write(id_file) ngrp_nod_filter_IO
      write(id_file) istack_nod_filter_IO(1:ngrp_nod_filter_IO)
      write(id_file) grp_name_filter_IO(1:ngrp_nod_filter_IO)
!
      write(id_file) inod_filter_IO(1:ntot_nod_filter_IO)
      write(id_file) istack_near_nod_filter_IO(1:ntot_nod_filter_IO)
!
      end subroutine write_3d_filter_stack_b
!
!  ---------------------------------------------------------------------
!
      subroutine write_3d_filter_weights_coef_b(id_file)
!
      integer(kind = kint), intent(in) :: id_file
!
!
      write(id_file) inod_near_nod_IO(1:ntot_near_nod_filter_IO)
      write(id_file) filter_func_IO(1:ntot_near_nod_filter_IO)
      write(id_file) filter_weight_IO(1:ntot_near_nod_filter_IO)
!
      end subroutine write_3d_filter_weights_coef_b
!
!  ---------------------------------------------------------------------
!
      end module filter_coef_IO
