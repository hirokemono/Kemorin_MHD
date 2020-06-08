!>@file   m_field_file_format.f90
!!@brief  module m_field_file_format
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2008
!
!>@brief Integer flags for field data file format
!!
!!
!!@verbatim
!!      subroutine choose_ucd_file_format(file_fmt_ctl, i_file_fmt,     &
!!     &          id_field_file_format)
!!      subroutine input_ucd_file_format_code(iflag_psf_fmt, file_head)
!!
!! ------------------------------------------------------------------
!!   flag lists for field data
!!
!!  Distributed data file
!!   ASCII   data file
!!     UCD file:             ucd_ascii
!!     splitted UCD file:    udt_ascii
!!     VTK file:             vtk_ascii
!!     splitted VTK file:    vtd_ascii
!!   BINARY  data file
!!     splitted file:        binary
!!
!!   GZIPPED data file
!!     UCD file:             ucd_gzip
!!     splitted UCD file:    udt_gzip
!!     VTK file:             vtk_gzip
!!     splitted VTK file:    vtd_gzip
!!
!!  Merged data file
!!   ASCII   data file
!!     UCD file:             single_ucd_ascii
!!     splitted UCD file:    single_udt_ascii
!!     VTK file:             single_vtk_ascii
!!     splitted VTK file:    single_vtd_ascii
!!     HDF5 file:            merged_HDF5
!!
!!   GZIPPED data file
!!     UCD file:             single_ucd_gzip
!!     splitted UCD file:    single_udt_gzip
!!     VTK file:             single_vtk_gzip
!!     splitted VTK file:    single_vtd_gzip
!!
!! ------------------------------------------------------------------
!!@endverbatim
!!
!!@n @param file_fmt_ctl   file format name (see above)
!!@n @param i_file_fmt     integer flag if file format is read
!!@n @param id_field_file_format   integer flag for file format
!
      module m_field_file_format
!
      use m_precision
      use m_file_format_switch
!
      implicit    none
!
!>      Integer flag for origianl ascii data format
      integer(kind = kint), parameter :: iflag_ascii                    &
     &                     = id_ascii_file_fmt
!>      Integer flag for origianl binary data format
      integer(kind = kint), parameter :: iflag_bin                      &
     &                     = id_binary_file_fmt
!>      Integer flag for origianl gzipped ascii data format
      integer(kind = kint), parameter :: iflag_gzip                     &
     &                     = id_gzip_txt_file_fmt
!>      Integer flag for gzipped binary data
      integer(kind = kint), parameter :: iflag_bin_gz                   &
     &                     = id_gzip_bin_file_fmt
!
!>      Integer flag for origianl ascii data format
      integer(kind = kint), parameter :: iflag_fld =       0
!>      Integer flag for UCD data
      integer(kind = kint), parameter :: iflag_ucd =      10
!>      Integer flag for UCD data
!!         (Separated by FEM mesh part and field part)
      integer(kind = kint), parameter :: iflag_udt =      20
!>      Integer flag for VTK data
      integer(kind = kint), parameter :: iflag_vtk =      30
!>      Integer flag for VTK data
!!         (Separated by FEM mesh part and field part)
      integer(kind = kint), parameter :: iflag_vtd =      40
!
!>      Integer flag for UCD data
      integer(kind = kint), parameter :: iflag_ucd_bin =  70
!>      Integer flag for UCD data
!!         (Separated by FEM mesh part and field part)
      integer(kind = kint), parameter :: iflag_udt_bin =  80
!
!>      Integer flag for merged binary data
      integer(kind = kint), parameter :: iflag_sgl_bin =  101
!>      Integer flag for merged binary data
      integer(kind = kint), parameter :: iflag_sgl_ucd_bin =  171
!>      Integer flag for merged binary data
      integer(kind = kint), parameter :: iflag_sgl_udt_bin =  181
!
!>      Integer flag for merged UCD data
      integer(kind = kint), parameter :: iflag_sgl_ucd =  110
!>      Integer flag for merged UCD data
!!         (Separated by FEM mesh part and field part)
      integer(kind = kint), parameter :: iflag_sgl_udt =  120
!>      Integer flag for merged VTK data
      integer(kind = kint), parameter :: iflag_sgl_vtk =  130
!>      Integer flag for merged VTK data
!!         (Separated by FEM mesh part and field part)
      integer(kind = kint), parameter :: iflag_sgl_vtd =  140
!>      Integer flag for HDF file data
      integer(kind = kint), parameter :: iflag_sgl_hdf5 = 150
!
!
!>      Integer flag for gzipped ascii original data
      integer(kind = kint), parameter :: iflag_fld_gz =       3
!>      Integer flag for gzipped UCD data
      integer(kind = kint), parameter :: iflag_ucd_gz =      13
!>      Integer flag for gzipped UCD data
!!         (Separated by FEM mesh part and field part)
      integer(kind = kint), parameter :: iflag_udt_gz =      23
!>      Integer flag for gzipped VTK data
      integer(kind = kint), parameter :: iflag_vtk_gz =      33
!>      Integer flag for gzipped VTK data
!!         (Separated by FEM mesh part and field part)
      integer(kind = kint), parameter :: iflag_vtd_gz =      43
!
!>      Integer flag for UCD data
      integer(kind = kint), parameter :: iflag_ucd_bin_gz =  12
!>      Integer flag for UCD data
!!         (Separated by FEM mesh part and field part)
      integer(kind = kint), parameter :: iflag_udt_bin_gz =  22
!
!>      Integer flag for merged gzipped binary data
      integer(kind = kint), parameter :: iflag_sgl_bin_gz = 102
!>      Integer flag for merged binary field and grid data
      integer(kind = kint), parameter :: iflag_sgl_ucd_bin_gz =  172
!>      Integer flag for merged binary field data
      integer(kind = kint), parameter :: iflag_sgl_udt_bin_gz =  182
!
!>      Integer flag for merged gzipped UCD data
      integer(kind = kint), parameter :: iflag_sgl_ucd_gz = 113
!>      Integer flag for merged gzipped UCD data
!!         (Separated by FEM mesh part and field part)
      integer(kind = kint), parameter :: iflag_sgl_udt_gz = 123
!>      Integer flag for merged amd gzipped VTK data
      integer(kind = kint), parameter :: iflag_sgl_vtk_gz = 133
!>      Integer flag for merged amd gzipped VTK data
!!         (Separated by FEM mesh part and field part)
      integer(kind = kint), parameter :: iflag_sgl_vtd_gz = 143
!
!
!
!>      flag for UCD
      character(len = kchara), parameter :: ucd_name = 'UCD'
!>      flag for Splitted UCD
      character(len = kchara), parameter :: udt_name = 'UDT'
!>      flag for VTK
      character(len = kchara), parameter :: vtk_name = 'VTK'
!>      flag for Splitted VTK
      character(len = kchara), parameter :: vtd_name = 'VTD'
!
!>      flag parts for compressed data
      character(len = kchara), parameter :: gzip_names(2)               &
     &                        = (/'gzip ', 'gz   '/)
!
      type multi_flag_labels
        integer(kind = kint) :: n_flag
        character(len = kchara), allocatable :: flags(:)
      end type multi_flag_labels
!
!>     Character lables for UCD
!!           1           4 gzip
!!           2           2 gz
      type(multi_flag_labels), save :: gzip_flags
!
!>     Character lables for UCD
!!           1           3 UCD
      type(multi_flag_labels), save :: ucd_flags
!>     Character lables for splitted UCD
!!           1           3 UDT
      type(multi_flag_labels), save :: udt_flags
!>     Character lables for VTK
!!           1           3 VTK
      type(multi_flag_labels), save :: vtk_flags
!>     Character lables for splitted VTK
!!           1           3 VTD
      type(multi_flag_labels), save :: vtd_flags
!
!>     Character lables for gzipped UCD
!!           1           8 UCD_gzip
!!           2           6 UCD_gz
!!           3           8 gzip_UCD
!!           4           6 gz_UCD
      type(multi_flag_labels), save :: ucd_gz_flags
!>     Character lables for gzipped splitted UCD
!!           1           8 UDT_gzip
!!           2           6 UDT_gz
!!           3           8 gzip_UDT
!!           4           6 gz_UDT
      type(multi_flag_labels), save :: udt_gz_flags
!>     Character lables for gzipped VTK
!!           1           8 VTK_gzip
!!           2           6 VTK_gz
!!           3           8 gzip_VTK
!!           4           6 gz_VTK
      type(multi_flag_labels), save :: vtk_gz_flags
!>     Character lables for gzipped splitted VTK
!!           1           8 VTD_gzip
!!           2           6 VTD_gz
!!           3           8 gzip_VTD
!!           4           6 gz_VTD
      type(multi_flag_labels), save :: vtd_gz_flags
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_gz_flags()
!
      gzip_flags%n_flag = 2
      call alloc_multi_flags(gzip_flags)
      gzip_flags%flags(1:2) = gzip_names(1:2)
!
      end subroutine init_gz_flags
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine init_ucd_flags()
!
      ucd_flags%n_flag = 1
      call alloc_multi_flags(ucd_flags)
      ucd_flags%flags(1) = trim(ADJUSTL(ucd_name))
!
      end subroutine init_ucd_flags
!
! -----------------------------------------------------------------------
!
      subroutine init_udt_flags()
!
      udt_flags%n_flag = 1
      call alloc_multi_flags(udt_flags)
      udt_flags%flags(1) = trim(ADJUSTL(udt_name))
!
      end subroutine init_udt_flags
!
! -----------------------------------------------------------------------
!
      subroutine init_vtk_flags()
!
      vtk_flags%n_flag = 1
      call alloc_multi_flags(vtk_flags)
      vtk_flags%flags(1) = trim(ADJUSTL(vtk_name))
!
      end subroutine init_vtk_flags
!
! -----------------------------------------------------------------------
!
      subroutine init_vtd_flags()
!
      vtd_flags%n_flag = 1
      call alloc_multi_flags(vtd_flags)
      vtd_flags%flags(1) = trim(ADJUSTL(vtd_name))
!
      end subroutine init_vtd_flags
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine init_ucd_gz_flags()
!
      integer(kind = kint) :: icou
!
      ucd_gz_flags%n_flag = 2 * gzip_flags%n_flag
      call alloc_multi_flags(ucd_gz_flags)
!
      icou = 0
      call connect_two_mul_flags                                        &
     &   (ucd_flags, gzip_flags, ucd_gz_flags, icou)
      call connect_two_mul_flags                                        &
     &   (gzip_flags, ucd_flags, ucd_gz_flags, icou)
!
      end subroutine init_ucd_gz_flags
!
! -----------------------------------------------------------------------
!
      subroutine init_udt_gz_flags()
!
      integer(kind = kint) :: icou
!
      udt_gz_flags%n_flag = 2 * gzip_flags%n_flag
      call alloc_multi_flags(udt_gz_flags)
!
      icou = 0
      call connect_two_mul_flags                                        &
     &   (udt_flags, gzip_flags, udt_gz_flags, icou)
      call connect_two_mul_flags                                        &
     &   (gzip_flags, udt_flags, udt_gz_flags, icou)
!
      end subroutine init_udt_gz_flags
!
! -----------------------------------------------------------------------
!
      subroutine init_vtk_gz_flags()
!
      integer(kind = kint) :: icou
!
      vtk_gz_flags%n_flag = 2 * gzip_flags%n_flag
      call alloc_multi_flags(vtk_gz_flags)
!
      icou = 0
      call connect_two_mul_flags                                        &
     &   (vtk_flags, gzip_flags, vtk_gz_flags, icou)
      call connect_two_mul_flags                                        &
     &   (gzip_flags, vtk_flags, vtk_gz_flags, icou)
!
      end subroutine init_vtk_gz_flags
!
! -----------------------------------------------------------------------
!
      subroutine init_vtd_gz_flags()
!
      integer(kind = kint) :: icou
!
      vtd_gz_flags%n_flag = 2 * gzip_flags%n_flag
      call alloc_multi_flags(vtd_gz_flags)
!
      icou = 0
      call connect_two_mul_flags                                        &
     &   (vtd_flags, gzip_flags, vtd_gz_flags, icou)
      call connect_two_mul_flags                                        &
     &   (gzip_flags, vtd_flags, vtd_gz_flags, icou)
!
      end subroutine init_vtd_gz_flags
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_multi_flags(mul_flags)
!
      type(multi_flag_labels), intent(inout) :: mul_flags
!
      if(allocated(mul_flags%flags)) deallocate(mul_flags%flags)
!
      allocate(mul_flags%flags(mul_flags%n_flag))
!
      end subroutine alloc_multi_flags
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_multi_flags(mul_flags)
!
      type(multi_flag_labels), intent(inout) :: mul_flags
!
      if(allocated(mul_flags%flags)) deallocate(mul_flags%flags)
!
      end subroutine dealloc_multi_flags
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function num_multi_flags(mul_flags)
!
      type(multi_flag_labels), intent(in) :: mul_flags
!
      num_multi_flags = mul_flags%n_flag
!
      end function num_multi_flags
!
! -----------------------------------------------------------------------
!
      subroutine set_multi_flags(mul_flags, flags)
!
      type(multi_flag_labels), intent(in) :: mul_flags
      character(len = kchara), intent(inout) :: flags(mul_flags%n_flag)
!
      integer(kind = kint) :: icou
!
      do icou = 1, mul_flags%n_flag
        write(flags(icou), '(a,a1)')                                    &
     &                     trim(mul_flags%flags(icou)), char(0)
      end do
!
      end subroutine set_multi_flags
!
! -----------------------------------------------------------------------
!
      subroutine connect_two_mul_flags                                  &
     &         (in_flags1, in_flags2, out_flags, icou)
!
      type(multi_flag_labels), intent(in) :: in_flags1
      type(multi_flag_labels), intent(in) :: in_flags2
      type(multi_flag_labels), intent(inout) :: out_flags
      integer(kind = kint), intent(inout) :: icou
!
      integer(kind = kint) :: i1, i2
!
      do i1 = 1, in_flags1%n_flag
        do i2 = 1, in_flags2%n_flag
          icou = icou + 1
          out_flags%flags(icou)                                         &
     &                   = trim(ADJUSTL(in_flags1%flags(i1))) // '_'    &
     &                  // trim(ADJUSTL(in_flags2%flags(i2)))
        end do
      end do
!
      end subroutine connect_two_mul_flags
!
! -----------------------------------------------------------------------
!
      logical function check_mul_flags(input_flag, mul_flags)
!
      use skip_comment_f
!
      character(len=kchara), intent(in) :: input_flag
      type(multi_flag_labels), intent(in) :: mul_flags
!
      integer(kind = kint) :: icou
!
!
      check_mul_flags = .FALSE.
      do icou = 1, mul_flags%n_flag
        check_mul_flags = check_mul_flags                               &
     &             .or. cmp_no_case(input_flag, mul_flags%flags(icou))
      end do
!
      end function check_mul_flags
!
! -----------------------------------------------------------------------
!
      subroutine write_multi_flags(id_file, title, mul_flags)
!
      integer(kind= kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: title
      type(multi_flag_labels), intent(in) :: mul_flags
!
      integer(kind = kint) :: icou
!
      write(id_file,*)
      write(id_file,*) trim(title)
      do icou = 1, mul_flags%n_flag
        write(id_file,*) icou, len_trim(mul_flags%flags(icou)),         &
     &                         trim(mul_flags%flags(icou))
      end do
!
      end subroutine write_multi_flags
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine choose_ucd_file_format(file_fmt_ctl, i_file_fmt,       &
     &          id_field_file_format)
!
      use skip_comment_f
!
      integer(kind= kint), intent(in) :: i_file_fmt
      character(len=kchara), intent(in) :: file_fmt_ctl
      integer(kind= kint), intent(inout) :: id_field_file_format
!
!
      if (i_file_fmt .eq. 0) then
        id_field_file_format = iflag_fld
        return
      end if
!
      call init_gz_flags()
!
      call init_ucd_flags()
      call init_udt_flags()
      call init_vtk_flags()
      call init_vtd_flags()
!
      call init_ucd_gz_flags()
      call init_udt_gz_flags()
      call init_vtk_gz_flags()
      call init_vtd_gz_flags()
!
      if     (cmp_no_case(file_fmt_ctl, hd_bin1)                        &
     &   .or. cmp_no_case(file_fmt_ctl, hd_bin2)         ) then
           id_field_file_format = iflag_bin
      else if(cmp_no_case(file_fmt_ctl, hd_bin_gz1)                     &
     &   .or. cmp_no_case(file_fmt_ctl, hd_bin_gz2)                     &
     &   .or. cmp_no_case(file_fmt_ctl, hd_bin_gz3)                     &
     &   .or. cmp_no_case(file_fmt_ctl, hd_bin_gz4)                     &
     &   .or. cmp_no_case(file_fmt_ctl, hd_bin_gz5)                     &
     &   .or. cmp_no_case(file_fmt_ctl, hd_bin_gz6)                     &
     &   .or. cmp_no_case(file_fmt_ctl, hd_bin_gz7)                     &
     &   .or. cmp_no_case(file_fmt_ctl, hd_bin_gz8) ) then
           id_field_file_format = iflag_bin_gz
      else if(cmp_no_case(file_fmt_ctl, hd_merge_bin1)                &
     &   .or. cmp_no_case(file_fmt_ctl, hd_merge_bin2)                &
     &   .or. cmp_no_case(file_fmt_ctl, hd_merge_bin3)                &
     &   .or. cmp_no_case(file_fmt_ctl, hd_merge_bin4)                &
     &   .or. cmp_no_case(file_fmt_ctl, hd_merge_bin5)                &
     &   .or. cmp_no_case(file_fmt_ctl, hd_merge_bin6)                &
     &   .or. cmp_no_case(file_fmt_ctl, hd_merge_bin7)                &
     &   .or. cmp_no_case(file_fmt_ctl, hd_merge_bin8) ) then
           id_field_file_format = iflag_sgl_bin
      else if(cmp_no_case(file_fmt_ctl, hd_merged_bin_gz1)            &
     &   .or. cmp_no_case(file_fmt_ctl, hd_merged_bin_gz2)            &
     &   .or. cmp_no_case(file_fmt_ctl, hd_merged_bin_gz3)            &
     &   .or. cmp_no_case(file_fmt_ctl, hd_merged_bin_gz4)            &
     &   .or. cmp_no_case(file_fmt_ctl, hd_merged_bin_gz5)            &
     &   .or. cmp_no_case(file_fmt_ctl, hd_merged_bin_gz6)            &
     &   .or. cmp_no_case(file_fmt_ctl, hd_merged_bin_gz7)            &
     &   .or. cmp_no_case(file_fmt_ctl, hd_merged_bin_gz8)            &
     &   .or. cmp_no_case(file_fmt_ctl, hd_merged_bin_gz9)            &
     &   .or. cmp_no_case(file_fmt_ctl, hd_merged_bin_gz10)           &
     &   .or. cmp_no_case(file_fmt_ctl, hd_merged_bin_gz11)           &
     &   .or. cmp_no_case(file_fmt_ctl, hd_merged_bin_gz12)           &
     &   .or. cmp_no_case(file_fmt_ctl, hd_merged_bin_gz13)           &
     &   .or. cmp_no_case(file_fmt_ctl, hd_merged_bin_gz14)           &
     &   .or. cmp_no_case(file_fmt_ctl, hd_merged_bin_gz15)           &
     &   .or. cmp_no_case(file_fmt_ctl, hd_merged_bin_gz16) ) then
           id_field_file_format = iflag_sgl_bin_gz
!
      else if(cmp_no_case(file_fmt_ctl, 'field_ascii')                  &
     &   .or. cmp_no_case(file_fmt_ctl, 'field')                        &
     &   .or. cmp_no_case(file_fmt_ctl, 'fld_ascii')                    &
     &   .or. cmp_no_case(file_fmt_ctl, 'fld')                          &
     &   .or. cmp_no_case(file_fmt_ctl, 'ascii_field')                  &
     &   .or. cmp_no_case(file_fmt_ctl, 'ascii_fld')                    &
     &   .or. cmp_no_case(file_fmt_ctl, 'ascii')                        &
     &   .or. cmp_no_case(file_fmt_ctl, 'text')        ) then
           id_field_file_format = iflag_fld
      else if(cmp_no_case(file_fmt_ctl, 'field_gzip')                   &
     &   .or. cmp_no_case(file_fmt_ctl, 'field_gz')                     &
     &   .or. cmp_no_case(file_fmt_ctl, 'fld_gzip')                     &
     &   .or. cmp_no_case(file_fmt_ctl, 'fld_gz')                       &
     &   .or. cmp_no_case(file_fmt_ctl, 'gzip_field')                   &
     &   .or. cmp_no_case(file_fmt_ctl, 'gz_field')                     &
     &   .or. cmp_no_case(file_fmt_ctl, 'gzip_fld')                     &
     &   .or. cmp_no_case(file_fmt_ctl, 'gz_fld')                       &
     &   .or. cmp_no_case(file_fmt_ctl, 'gzip')                         &
     &   .or. cmp_no_case(file_fmt_ctl, 'gz')         ) then
           id_field_file_format = iflag_fld + iflag_gzip
      else if(check_mul_flags(file_fmt_ctl, udt_flags)) then
           id_field_file_format = iflag_udt
      else if(check_mul_flags(file_fmt_ctl, udt_gz_flags)) then
           id_field_file_format = iflag_udt + iflag_gzip
!
      else if(check_mul_flags(file_fmt_ctl, ucd_flags)) then
           id_field_file_format = iflag_ucd
      else if(check_mul_flags(file_fmt_ctl, ucd_gz_flags)) then
           id_field_file_format = iflag_ucd + iflag_gzip
!
      else if(check_mul_flags(file_fmt_ctl, vtd_flags)) then
           id_field_file_format = iflag_vtd
      else if(check_mul_flags(file_fmt_ctl, vtd_gz_flags)) then
           id_field_file_format = iflag_vtd + iflag_gzip
!
      else if(check_mul_flags(file_fmt_ctl, vtk_flags)) then
           id_field_file_format = iflag_vtk
      else if(check_mul_flags(file_fmt_ctl, vtk_gz_flags)) then
           id_field_file_format = iflag_vtk + iflag_gzip
      else
           id_field_file_format = iflag_udt
      end if
!
      call dealloc_multi_flags(gzip_flags)
!
      call dealloc_multi_flags(ucd_flags)
      call dealloc_multi_flags(udt_flags)
      call dealloc_multi_flags(vtk_flags)
      call dealloc_multi_flags(vtd_flags)
!
      call dealloc_multi_flags(ucd_gz_flags)
      call dealloc_multi_flags(udt_gz_flags)
      call dealloc_multi_flags(vtk_gz_flags)
      call dealloc_multi_flags(vtd_gz_flags)
!
      end subroutine choose_ucd_file_format
!
! -----------------------------------------------------------------------
!
      subroutine input_ucd_file_format_code(iflag_psf_fmt, file_head)
!
      integer(kind = kint), intent(inout) :: iflag_psf_fmt
      character(len=kchara), intent(inout) :: file_head
!
!
      write(*,*) 'Choose psf format'
      write(*,*) iflag_ucd, ': UCD'
      write(*,*) iflag_udt, ': UDT'
      write(*,*) iflag_vtk, ': VTK'
!      write(*,*) iflag_vtd, ': VTD'
      write(*,*) iflag_ucd_gz, ': gzipped_UCD'
      write(*,*) iflag_udt_gz, ': gzipped_UDT'
      write(*,*) iflag_vtk_gz, ': gzipped_VTK'
!      write(*,*) iflag_vtd_gz, ': gzipped_VTD'
!
      read(*,*)  iflag_psf_fmt
      write(*,*) 'input file format code: ', iflag_psf_fmt
!
      write(*,*) 'input file prefix'
      read(*,*) file_head
!
      end subroutine input_ucd_file_format_code
!
! -----------------------------------------------------------------------
!
      end module m_field_file_format
