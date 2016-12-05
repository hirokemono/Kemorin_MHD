!>@file   t_element_refinement_IO.f90
!!@brief  module t_element_refinement_IO
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2009
!
!>@brief Array for element refinement table data
!!
!!@verbatim
!!      subroutine alloc_element_refine_IO(e_ref_IO)
!!      subroutine dealloc_element_refine_IO(e_ref_IO)
!!
!!      subroutine write_element_refine_data(id_file, e_ref_IO)
!!      subroutine read_element_refine_data(id_file, e_ref_IO)
!!
!!      subroutine write_element_refine_data_b(e_ref_IO)
!!      subroutine read_element_refine_data_b(e_ref_IO)
!!@endverbatim
!
      module t_element_refinement_IO
!
      use m_precision
!
      implicit  none
!
      type ele_refine_IO_type
        character(len = kchara) :: file_head = 'refine_info'
!
        integer(kind = kint) :: max_refine_level
        integer(kind = kint) :: nele_ref
        integer(kind = kint) :: nele_org
        integer(kind = kint), allocatable :: iele_gl_new(:)
        integer(kind = kint), allocatable :: ilevel_refine(:)
        integer(kind = kint), allocatable :: iflag_refine_ele(:)
        integer(kind = kint), allocatable :: iele_gl_org(:)
        integer(kind = kint), allocatable :: icou_gl_org(:)
      end type ele_refine_IO_type
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_element_refine_IO(e_ref_IO)
!
      type(ele_refine_IO_type), intent(inout) :: e_ref_IO
!
!
      allocate(e_ref_IO%iele_gl_new(e_ref_IO%nele_ref))
      allocate(e_ref_IO%ilevel_refine(e_ref_IO%nele_ref))
      allocate(e_ref_IO%iflag_refine_ele(e_ref_IO%nele_ref))
      allocate(e_ref_IO%iele_gl_org(e_ref_IO%nele_ref))
      allocate(e_ref_IO%icou_gl_org(e_ref_IO%nele_ref))
!
      if(e_ref_IO%nele_ref .le. 0) return
        e_ref_IO%iele_gl_new =      0
        e_ref_IO%ilevel_refine =    0
        e_ref_IO%iflag_refine_ele = 0
        e_ref_IO%iele_gl_org =      0
        e_ref_IO%icou_gl_org =      0
!
      end subroutine alloc_element_refine_IO
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_element_refine_IO(e_ref_IO)
!
      type(ele_refine_IO_type), intent(inout) :: e_ref_IO
!
!
      deallocate(e_ref_IO%iele_gl_new)
      deallocate(e_ref_IO%ilevel_refine, e_ref_IO%iflag_refine_ele)
      deallocate(e_ref_IO%iele_gl_org, e_ref_IO%icou_gl_org)
!
      end subroutine dealloc_element_refine_IO
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine write_element_refine_data(id_file, e_ref_IO)
!
      integer(kind = kint), intent(in) :: id_file
      type(ele_refine_IO_type), intent(in) :: e_ref_IO
!
      integer(kind = kint) :: iele_neo
!
!
      write(id_file,'(a)') trim('!')
      write(id_file,'(a)') trim('! maximum_refine_level')
      write(id_file,'(4i16)') e_ref_IO%max_refine_level
      write(id_file,'(a)') trim('!')
      write(id_file,'(a)') trim('! org_num_ele, new_num_ele')
      write(id_file,'(2i16)')                                           &
     &       e_ref_IO%nele_ref, e_ref_IO%nele_org
!
      write(id_file,'(a)') trim('!')
      write(id_file,'(2a)')                                             &
     &  trim('! global_ele_ID, org_ele_ID, '),                          &
     &  trim('refine_type_ID, local_refined_idx, reine_level')
      write(id_file,'(a)') trim('!')
      write(id_file,'(a)') trim('!')
!
      write(id_file,'(i16)')  e_ref_IO%nele_ref
      do iele_neo = 1, e_ref_IO%nele_ref
        write(id_file,'(6i16)') e_ref_IO%iele_gl_new(iele_neo),         &
     &       e_ref_IO%ilevel_refine(iele_neo),                          &
     &       e_ref_IO%iflag_refine_ele(iele_neo),                       &
     &       e_ref_IO%iele_gl_org(iele_neo),                            &
     &       e_ref_IO%icou_gl_org(iele_neo)
      end do
!
      end subroutine write_element_refine_data
!
! ----------------------------------------------------------------------
!
      subroutine read_element_refine_data(id_file, e_ref_IO)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      type(ele_refine_IO_type), intent(inout) :: e_ref_IO
!
      integer(kind = kint) :: iele_neo
      character(len=255) :: character_4_read
!
!
      call skip_comment(character_4_read, id_file)
      read(character_4_read,*) e_ref_IO%nele_ref, e_ref_IO%nele_org
!
      call skip_comment(character_4_read, id_file)
      read(character_4_read,*) e_ref_IO%nele_ref
      call alloc_element_refine_IO(e_ref_IO)
!
      do iele_neo = 1, e_ref_IO%nele_ref
        read(id_file,*) e_ref_IO%iele_gl_new(iele_neo),                 &
     &       e_ref_IO%ilevel_refine(iele_neo),                          &
     &       e_ref_IO%iflag_refine_ele(iele_neo),                       &
     &       e_ref_IO%iele_gl_org(iele_neo),                            &
     &       e_ref_IO%icou_gl_org(iele_neo)
      end do
!
      end subroutine read_element_refine_data
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine write_element_refine_data_b(e_ref_IO)
!
      use binary_IO
!
      type(ele_refine_IO_type), intent(in) :: e_ref_IO
!
!
      call write_one_integer_b(e_ref_IO%max_refine_level)
      call write_one_integer_b(e_ref_IO%nele_ref)
      call write_one_integer_b(e_ref_IO%nele_org)
!
      call write_mul_integer_b                                          &
     &    (e_ref_IO%nele_ref, e_ref_IO%iele_gl_new)
      call write_mul_integer_b                                          &
     &   (e_ref_IO%nele_ref, e_ref_IO%ilevel_refine)
      call write_mul_integer_b                                          &
     &   (e_ref_IO%nele_ref, e_ref_IO%iflag_refine_ele)
      call write_mul_integer_b                                          &
     &   (e_ref_IO%nele_ref, e_ref_IO%iele_gl_org)
      call write_mul_integer_b                                          &
     &   (e_ref_IO%nele_ref, e_ref_IO%icou_gl_org)
!
      end subroutine write_element_refine_data_b
!
! ----------------------------------------------------------------------
!
      subroutine read_element_refine_data_b(e_ref_IO)
!
      use binary_IO
!
      type(ele_refine_IO_type), intent(inout) :: e_ref_IO
!
!
      call read_one_integer_b(e_ref_IO%max_refine_level)
      call read_one_integer_b(e_ref_IO%nele_ref)
      call read_one_integer_b(e_ref_IO%nele_org)
!
      call alloc_element_refine_IO(e_ref_IO)
!
      call read_mul_integer_b                                           &
     &   (e_ref_IO%nele_ref, e_ref_IO%iele_gl_new)
      call read_mul_integer_b                                           &
     &   (e_ref_IO%nele_ref, e_ref_IO%ilevel_refine)
      call read_mul_integer_b                                           &
     &   (e_ref_IO%nele_ref, e_ref_IO%iflag_refine_ele)
      call read_mul_integer_b                                           &
     &   (e_ref_IO%nele_ref, e_ref_IO%iele_gl_org)
      call read_mul_integer_b                                           &
     &   (e_ref_IO%nele_ref, e_ref_IO%icou_gl_org)
!
      end subroutine read_element_refine_data_b
!
! ----------------------------------------------------------------------
!
      end module t_element_refinement_IO
