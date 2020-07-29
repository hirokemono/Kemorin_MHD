!
!
      program tako
!
      use m_precision
      use m_file_format_switch
      use m_field_file_format_labels
      use m_merged_field_fmt_labels
!
      implicit none
!
!>     Character lables for merged ASCII
!!        'merged_ascii', 'merged_text', 'single_ascii', 'single_text',
!!        'ascii_merged', 'ascii_single', 'text_merged', 'text_single',
!!        'merged', 'single' 
      type(multi_flag_labels), save :: mgd_ascii_flags
!>     Character lables for merged binary
!!        'merged_binary', 'merged_bin', 'single_binary', 'single_bin',
!!        'binary_merged', 'binary_single', 'bin_merged', 'bin_single' 
      type(multi_flag_labels), save :: mgd_binary_flags
!>     Character lables for merged compressed ASCII
!!        'merged_ascii_gzip', 'merged_ascii_gz',   'merged_text_gzip',
!!        'merged_text_gz',    'merged_gzip_ascii', 'merged_gzip_text',
!!        'merged_gz_ascii',   'merged_gz_text',    'merged_gzip',
!!        'merged_gz',         'single_ascii_gzip', 'single_ascii_gz',
!!        'single_text_gzip',  'single_text_gz',    'single_gzip_ascii',
!!        'single_gzip_text',  'single_gz_ascii',   'single_gz_text',
!!        'single_gzip',       'single_gz',         'ascii_gzip_merged',
!!        'ascii_gzip_single', 'ascii_gz_merged',   'ascii_gz_single',
!!        'text_gzip_merged',  'text_gzip_single',  'text_gz_merged',
!!        'text_gz_single',    'gzip_ascii_merged', 'gzip_ascii_single',
!!        'gzip_text_merged',  'gzip_text_single',  'gz_ascii_merged',
!!        'gz_ascii_single',   'gz_text_merged',    'gz_text_single',
!!        'gzip_merged', 'gzip_single', 'gz_merged', 'gz_single' 
      type(multi_flag_labels), save :: mgd_gzip_flags
!>     Character lables for compressesd merged binary
!!        'merged_binary_gzip', 'merged_binary_gz',   'merged_bin_gzip',
!!        'merged_bin_gz',      'merged_gzip_binary', 'merged_gzip_bin',
!!        'merged_gz_binary',   'merged_gz_bin',   'single_binary_gzip',
!!        'single_binary_gz',   'single_bin_gzip', 'single_bin_gz',
!!        'single_gzip_binary', 'single_gzip_bin', 'single_gz_binary',
!!        'single_gz_bin', 'binary_gzip_merged',   'binary_gzip_single',
!!        'binary_gz_merged',   'binary_gz_single',   'bin_gzip_merged',
!!        'bin_gzip_single',    'bin_gz_merged',      'bin_gz_single',
!!        'gzip_binary_merged', 'gzip_binary_single', 'gzip_bin_merged',
!!        'gzip_bin_single',    'gz_binary_merged',   'gz_binary_single',
!!        'gz_bin_merged',      'gz_bin_single' 
      type(multi_flag_labels), save :: mgd_gzip_bin_flags
!
      character(len = kchara) :: input_flag
!
      integer(kind = kint) :: icou
      integer(kind = kint) :: i1, i2, i3
!
!
!
      call init_mgd_field_type_flags()
!
      input_flag = 'ascii_flags'
      call write_multi_flags(6, input_flag, ascii_flags)
      input_flag = 'binary_flags'
      call write_multi_flags(6, input_flag, binary_flags)

      input_flag = 'gzip_ascii_flags'
      call write_multi_flags(6, input_flag, gzip_ascii_flags)
      input_flag = 'gzip_bin_flags'
      call write_multi_flags(6, input_flag, gzip_bin_flags)
!
      input_flag = 'mgd_ascii_flags'
      call write_multi_flags(6, input_flag, mgd_ascii_flags)
      input_flag = 'mgd_gzip_flags'
      call write_multi_flags(6, input_flag, mgd_gzip_flags)
!
      input_flag = 'mgd_binary_flags'
      call write_multi_flags(6, input_flag, mgd_binary_flags)
      input_flag = 'mgd_gzip_bin_flags'
      call write_multi_flags(6, input_flag, mgd_gzip_bin_flags)
!
      input_flag = 'gzip_flags'
      call write_multi_flags(6, input_flag, gzip_flags)
!
      input_flag = 'ucd_flags'
      call write_multi_flags(6, input_flag, ucd_flags)
!
      input_flag = 'udt_flags'
      call write_multi_flags(6, input_flag, udt_flags)
!
      input_flag = 'vtk_flags'
      call write_multi_flags(6, input_flag, vtk_flags)
!
      input_flag = 'vtd_flags'
      call write_multi_flags(6, input_flag, vtd_flags)
!
      input_flag = 'iso_flags'
      call write_multi_flags(6, input_flag, iso_flags)
!
      input_flag = 'psf_flags'
      call write_multi_flags(6, input_flag, psf_flags)
!
      input_flag = 'field_labels'
      call write_multi_flags(6, input_flag, field_labels)
!
      input_flag = 'ascii_flags'
      call write_multi_flags(6, input_flag, ascii_flags)
!
      input_flag = 'field_ascii_labels'
      call write_multi_flags(6, input_flag, field_ascii_labels)
!
      input_flag = 'field_bin_labels'
      call write_multi_flags(6, input_flag, field_bin_labels)
!
      input_flag = 'field_gz_labels'
      call write_multi_flags(6, input_flag, field_gz_labels)
!
      input_flag = 'fbin_gz_labels'
      call write_multi_flags(6, input_flag, fbin_gz_labels)
!
      input_flag = 'ucd_gz_flags'
      call write_multi_flags(6, input_flag, ucd_gz_flags)
!
      input_flag = 'udt_gz_flags'
      call write_multi_flags(6, input_flag, udt_gz_flags)
!
      input_flag = 'vtk_gz_flags'
      call write_multi_flags(6, input_flag, vtk_gz_flags)
!
      input_flag = 'vtd_gz_flags'
      call write_multi_flags(6, input_flag, vtd_gz_flags)
!
      input_flag = 'iso_gz_flags'
      call write_multi_flags(6, input_flag, iso_gz_flags)
!
      input_flag = 'psf_gz_flags'
      call write_multi_flags(6, input_flag, psf_gz_flags)
!
      input_flag = 'merged_flags'
      call write_multi_flags(6, input_flag, merged_flags)
!
!
      input_flag = 'mgd_fld_gz_labels'
      call write_multi_flags(6, input_flag, mgd_fld_gz_labels)
!
      input_flag = 'mgd_fbin_gz_labels'
      call write_multi_flags(6, input_flag, mgd_fbin_gz_labels)
!
!
      input_flag = 'mgd_ucd_labels'
      call write_multi_flags(6, input_flag, mgd_ucd_labels)
!
      input_flag = 'mgd_udt_labels'
      call write_multi_flags(6, input_flag, mgd_udt_labels)
!
      input_flag = 'mgd_vtk_labels'
      call write_multi_flags(6, input_flag, mgd_vtk_labels)
!
      input_flag = 'mgd_vtd_labels'
      call write_multi_flags(6, input_flag, mgd_vtd_labels)
!
      input_flag = 'mgd_iso_labels'
      call write_multi_flags(6, input_flag, mgd_iso_labels)
!
      input_flag = 'mgd_psf_labels'
      call write_multi_flags(6, input_flag, mgd_psf_labels)
!
      input_flag = 'mgd_hdf_labels'
      call write_multi_flags(6, input_flag, mgd_hdf_labels)
!
!
!
      input_flag = 'mgd_ucd_gz_labels'
      call write_multi_flags(6, input_flag, mgd_ucd_gz_labels)
!
      input_flag = 'mgd_udt_gz_labels'
      call write_multi_flags(6, input_flag, mgd_udt_gz_labels)
!
      input_flag = 'mgd_vtk_gz_labels'
      call write_multi_flags(6, input_flag, mgd_vtk_gz_labels)
!
      input_flag = 'mgd_vtd_gz_labels'
      call write_multi_flags(6, input_flag, mgd_vtd_gz_labels)
!
      input_flag = 'mgd_iso_gz_labels'
      call write_multi_flags(6, input_flag, mgd_iso_gz_labels)
!
      input_flag = 'mgd_psf_gz_labels'
      call write_multi_flags(6, input_flag, mgd_psf_gz_labels)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      end program tako

