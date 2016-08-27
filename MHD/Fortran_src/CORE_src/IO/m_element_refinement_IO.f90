!>@file   m_element_refinement_IO.f90
!!@brief  module m_element_refinement_IO
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2009
!
!>@brief Array for element refinement table data
!!
!!@verbatim
!!      subroutine allocate_element_refine_IO
!!      subroutine deallocate_element_refine_IO
!!
!!      subroutine write_element_refine_data(id_file)
!!      subroutine read_element_refine_data(id_file)
!!@endverbatim
!
      module m_element_refinement_IO
!
      use m_precision
!
      implicit  none
!
      integer(kind = kint), parameter :: id_refine_table = 19
      character(len = kchara) :: refine_info_fhead = 'refine_info'
      character(len = kchara) :: refine_info_fname = 'refine_info.dat'
!
      integer(kind = kint) :: max_refine_level_IO
      integer(kind = kint) :: nele_ref_IO, nele_org_IO
      integer(kind = kint), allocatable :: iele_global_new_IO(:)
      integer(kind = kint), allocatable :: ilevel_refine_IO(:)
      integer(kind = kint), allocatable :: iflag_refine_ele_IO(:)
      integer(kind = kint), allocatable :: iele_global_org_IO(:)
      integer(kind = kint), allocatable :: icou_global_org_IO(:)
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_element_refine_IO
!
!
      allocate(iele_global_new_IO(nele_ref_IO))
      allocate(ilevel_refine_IO(nele_ref_IO))
      allocate(iflag_refine_ele_IO(nele_ref_IO))
      allocate(iele_global_org_IO(nele_ref_IO))
      allocate(icou_global_org_IO(nele_ref_IO))
!
      if(nele_ref_IO .gt. 0) then
        iele_global_new_IO =  0
        ilevel_refine_IO =    0
        iflag_refine_ele_IO = 0
        iele_global_org_IO =  0
        icou_global_org_IO =  0
      end if
!
      end subroutine allocate_element_refine_IO
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_element_refine_IO
!
!
      deallocate(iele_global_new_IO)
      deallocate(ilevel_refine_IO, iflag_refine_ele_IO)
      deallocate(iele_global_org_IO, icou_global_org_IO)
!
      end subroutine deallocate_element_refine_IO
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine write_element_refine_data(id_file)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint) :: iele_neo
!
!
      write(id_refine_table,'(a)') trim('!')
      write(id_refine_table,'(a)') trim('! maximum_refine_level')
      write(id_refine_table,'(4i16)') max_refine_level_IO
      write(id_refine_table,'(a)') trim('!')
      write(id_refine_table,'(a)') trim('! org_num_ele, new_num_ele')
      write(id_refine_table,'(2i16)') nele_ref_IO, nele_org_IO
!
      write(id_refine_table,'(a)') trim('!')
      write(id_refine_table,'(2a)')                                     &
     &  trim('! global_ele_ID, org_ele_ID, '),                          &
     &  trim('refine_type_ID, local_refined_idx, reine_level')
      write(id_refine_table,'(a)') trim('!')
      write(id_refine_table,'(a)') trim('!')
!
      write(id_refine_table,'(i16)')  nele_ref_IO
      do iele_neo = 1, nele_ref_IO
        write(id_refine_table,'(6i16)') iele_global_new_IO(iele_neo),   &
     &       ilevel_refine_IO(iele_neo), iflag_refine_ele_IO(iele_neo), &
     &       iele_global_org_IO(iele_neo), icou_global_org_IO(iele_neo)
      end do
!
      end subroutine write_element_refine_data
!
! ----------------------------------------------------------------------
!
      subroutine read_element_refine_data(id_file)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint) :: iele_neo
      character(len=255) :: character_4_read
!
!
      call skip_comment(character_4_read, id_refine_table)
      read(character_4_read,*) nele_ref_IO, nele_org_IO
!
      call skip_comment(character_4_read, id_refine_table)
      read(character_4_read,*) nele_ref_IO
      call allocate_element_refine_IO
!
      do iele_neo = 1, nele_ref_IO
        read(id_refine_table,*) iele_global_new_IO(iele_neo),           &
     &       ilevel_refine_IO(iele_neo), iflag_refine_ele_IO(iele_neo), &
     &       iele_global_org_IO(iele_neo), icou_global_org_IO(iele_neo)
      end do
!
      end subroutine read_element_refine_data
!
! ----------------------------------------------------------------------
!
      end module m_element_refinement_IO
